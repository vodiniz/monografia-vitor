module ArtifactExport where

import Prelude

import ConcreteReplay
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.SBV.Dynamic as S
import qualified Data.Tree as T
import Data.Word (Word32)
import qualified Foundation as F
import PyEval (evalPyProgramWithBudget)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (
    dropExtension,
    takeBaseName,
    takeFileName,
    (</>),
 )
import Text.Read (readMaybe)

import PyCompiler (compileProgramWithInfo)
import PySyntax
import SMT
import Symbolic
import Types

data ExportTestCase = ExportTestCase
    { testId :: Int
    , inputAssignments :: [(String, Word32)]
    , inputOccurrences :: [(ReadEvent, String, Word32)]
    , expectedOutput :: [String]
    , expectedBranches :: [BranchEvent]
    , pathConditionText :: String
    , notes :: String
    }
    deriving (Eq, Show)

-- Retain the original entry point; callers that choose limits should use the
-- explicit version so the JSON and README record the actual configuration.
exportArtifacts :: FilePath -> String -> PyProgram -> [Instr] -> T.Tree SolvedState -> IO FilePath
exportArtifacts inputPath source ast instrs solved =
    exportArtifactsWith defaultAnalysisConfig (snd (compileProgramWithInfo ast)) inputPath source ast instrs solved

exportArtifactsWith :: AnalysisConfig -> ProgramInfo -> FilePath -> String -> PyProgram -> [Instr] -> T.Tree SolvedState -> IO FilePath
exportArtifactsWith config info inputPath source ast instrs solvedTraces = do
    let programName = programFolderName inputPath
        outputDir = "generated-tests" </> programName
        (testCases, rejected) = buildTestCases config info ast instrs solvedTraces
        -- Keep the existing filename convention to preserve downstream scripts.
        prefix = programName ++ "_"
        origFile = prefix ++ "original.py"
        stackFile = prefix ++ "stack_program.txt"
        txtFile = prefix ++ "test_cases.txt"
        jsonFile = prefix ++ "test_cases.json"
        runnerFile = prefix ++ "run_tests.py"
        readmeFile = prefix ++ "README.md"
        analysisFile = prefix ++ "analysis.json"
    createDirectoryIfMissing True outputDir
    writeFile (outputDir </> origFile) source
    writeFile (outputDir </> stackFile) (renderStackProgram instrs)
    writeFile (outputDir </> txtFile) (renderTestCases testCases)
    writeFile (outputDir </> jsonFile) (renderTestCasesJson testCases)
    writeFile (outputDir </> runnerFile) (replayScript origFile jsonFile)
    writeFile (outputDir </> analysisFile) (renderAnalysisJson config solvedTraces rejected)
    writeFile
        (outputDir </> readmeFile)
        ( renderReadme inputPath programName origFile stackFile txtFile jsonFile runnerFile readmeFile
            ++ "\n## Bounded exploration\n\n"
            ++ "loop-k: "
            ++ show (loopK config)
            ++ "; max-depth: "
            ++ show (maxDepth config)
            ++ ".\n"
            ++ "Only completed SAT paths with a successful concrete validation become tests.\n"
            ++ "See `"
            ++ analysisFile
            ++ "` for cuts, solver outcomes, and rejected models.\n"
            ++ "Word32 solver arithmetic is retained; this is not full Python integer semantics.\n"
            ++ "Models are validated against concrete compiled branches before export; replay checks source output and input consumption with a timeout.\n"
        )
    putStrLn ("Validated tests: " ++ show (length testCases) ++ "; rejected models: " ++ show (length rejected))
    return outputDir

programFolderName :: FilePath -> String
programFolderName path =
    let base = dropExtension (takeFileName path)
        cleaned = sanitizeName base
     in if null cleaned then "program" else cleaned

-- Use only portable filename characters. This makes output paths stable across
-- shells and operating systems without depending on the original directory path.
sanitizeName :: String -> String
sanitizeName =
    map replaceUnsafe
  where
    replaceUnsafe c
        | Char.isAlphaNum c = c
        | c == '-' = c
        | c == '_' = c
        | otherwise = '_'

-- Static collection remains useful as an inventory, but it no longer defines
-- input order: only the Read events on a particular path can provide that order.
collectInputs :: PyProgram -> [String]
collectInputs (PyProgram stmts) = concatMap collectStmtInputs stmts

collectStmtInputs :: PyStmt -> [String]
collectStmtInputs (PyInput name) = [name]
collectStmtInputs (PyPrint _) = []
collectStmtInputs (PyAssign _ _) = []
collectStmtInputs (PyIf _ yes no) = concatMap collectStmtInputs (yes ++ maybe [] id no)
collectStmtInputs (PyWhile _ body) = concatMap collectStmtInputs body
collectStmtInputs (PyForRange _ _ _ body) = concatMap collectStmtInputs body

buildTestCases :: AnalysisConfig -> ProgramInfo -> PyProgram -> [Instr] -> T.Tree SolvedState -> ([ExportTestCase], [String])
buildTestCases config info ast instrs solvedTraces =
    let candidates = filter isSatisfiableSolvedState (leaves solvedTraces)
        attempts = map mkTestCase candidates
        accepted = [value | Right value <- attempts]
        rejected = [err | Left err <- attempts]
     in (zipWith (\n value -> value{testId = n}) [1 ..] accepted, rejected)
  where
    -- An SMT model is necessary but insufficient: replay the compiled path with
    -- unbounded arithmetic and compare it to the expected symbolic decisions.
    -- This rejects models that exploit the existing Word32/Python mismatch.
    mkTestCase (SolvedState node (Just smtResult)) = do
        let (pc, _, _, _, cs, ctx) = traceState node
            model = modelAssignments smtResult
            events = reverse (readEvents ctx)
            sourceName event = M.findWithDefault ("val_" ++ show (readIndex event)) (readPc event) (programReads info)
            occurrences = [(event, sourceName event, M.findWithDefault 0 ("val_" ++ show (readIndex event)) model) | event <- events]
            values = [value | (_, _, value) <- occurrences]
            names = [name | (_, name, _) <- occurrences]
            label event name = if length (filter (== name) names) > 1 then name ++ "#" ++ show (readIndex event) else name
            assignments = [(label event name, value) | (event, name, value) <- occurrences]
            branches = reverse (branchEvents ctx)
            budget = max 1000 (maxDepth config * 2 + 10)
            -- Add the current program counter to validation errors while preserving
            -- the successful result type. The explicit polymorphic signature allows
            -- this helper to handle both source outputs and concrete replay results.
            annotate :: Either String a -> Either String a
            annotate (Left err) =
                Left ("PC " ++ show pc ++ ": " ++ err)
            annotate (Right value) =
                Right value
        expected <- annotate (evalPyProgramWithBudget budget ast values)
        concrete <- annotate (replayConcrete budget instrs values)
        if replayBranches concrete /= branches
            then Left ("PC " ++ show pc ++ ": concrete branch trace differs from the symbolic path (possible Word32 overflow).")
            else
                if replayOutput concrete /= expected
                    then Left ("PC " ++ show pc ++ ": compiled output differs from source evaluation.")
                    else
                        Right
                            ExportTestCase
                                { testId = 0
                                , inputAssignments = assignments
                                , inputOccurrences = occurrences
                                , expectedOutput = expected
                                , expectedBranches = branches
                                , pathConditionText = F.toList (renderSym (foldr SAnd (SCon 1) cs))
                                , notes = "Validated completed path at PC " ++ show pc
                                }
    mkTestCase _ = Left "A non-SAT path was passed to the test builder."

leaves :: T.Tree a -> [a]
leaves (T.Node x []) = [x]
leaves (T.Node _ children) = concatMap leaves children

-- A cut is never a completed program, even when its accumulated condition is SAT.
isSatisfiableSolvedState :: SolvedState -> Bool
isSatisfiableSolvedState (SolvedState node (Just (S.Satisfiable _ _))) = stopReason node == Completed
isSatisfiableSolvedState _ = False

modelAssignments :: S.SMTResult -> M.Map String Word32
modelAssignments result@(S.Satisfiable _ _) =
    M.mapMaybe parseModelWord (M.mapKeys F.toList (S.getModelDictionary result))
modelAssignments _ = M.empty

parseModelWord :: (Show a) => a -> Maybe Word32
parseModelWord value = readMaybe (takeWhile Char.isDigit (show value))

-- Report structural terminal states separately from solver feasibility. A cut
-- with an UNSAT condition must not be counted as a feasible uncovered path.
renderAnalysisJson :: AnalysisConfig -> T.Tree SolvedState -> [String] -> String
renderAnalysisJson config tree rejected =
    let terminals = leaves tree
        resultName Nothing = "not_queried"
        resultName (Just (S.Satisfiable _ _)) = "sat"
        resultName (Just (S.Unsatisfiable _ _)) = "unsat"
        resultName _ = "unknown_or_error"
        renderTerminal (SolvedState node result) =
            let (pc, _, _, _, _, _) = traceState node
             in "{\"pc\":"
                    ++ show pc
                    ++ ",\"status\":"
                    ++ jsonString (show (stopReason node))
                    ++ ",\"solver\":"
                    ++ jsonString (resultName result)
                    ++ "}"
     in "{\n  \"loop_k\": "
            ++ show (loopK config)
            ++ ",\n  \"max_depth\": "
            ++ show (maxDepth config)
            ++ ",\n  \"states\": "
            ++ show (length (T.flatten tree))
            ++ ",\n  \"solver_queries\": "
            ++ show (length terminals)
            ++ ",\n  \"terminal_paths\": ["
            ++ intercalate "," (map renderTerminal terminals)
            ++ "]"
            ++ ",\n  \"validation_rejections\": "
            ++ renderJsonStringList rejected
            ++ "\n}\n"

renderStackProgram :: [Instr] -> String
renderStackProgram instrs =
    unlines $
        [ "Generated stack-language program"
        , ""
        ]
            ++ zipWith renderInstr [0 :: Int ..] instrs
  where
    renderInstr pc instr = padLeft 4 (show pc) ++ ": " ++ show instr

renderTestCases :: [ExportTestCase] -> String
renderTestCases [] =
    "No satisfiable test cases were generated.\n"
renderTestCases testCases =
    intercalate "\n" (map renderTestCase testCases)

renderTestCase :: ExportTestCase -> String
renderTestCase testCase =
    unlines
        [ "=== Test Case " ++ show (testId testCase) ++ " ==="
        , "Inputs:"
        , renderInputs (inputAssignments testCase)
        , "Expected output:"
        , renderExpectedOutput (expectedOutput testCase)
        , ""
        , "Path condition:"
        , "  " ++ pathConditionText testCase
        , ""
        , "Notes:"
        , "  " ++ notes testCase
        ]

renderInputs :: [(String, Word32)] -> String
renderInputs [] =
    "  <none>"
renderInputs inputs =
    unlines ["  " ++ name ++ " = " ++ show value | (name, value) <- inputs]

renderExpectedOutput :: [String] -> String
renderExpectedOutput [] =
    "  <no output>"
renderExpectedOutput outputs =
    unlines ["  " ++ value | value <- outputs]

renderTestCasesJson :: [ExportTestCase] -> String
renderTestCasesJson testCases =
    "{\n"
        ++ "  \"test_cases\": [\n"
        ++ intercalate ",\n" (map renderJsonTestCase testCases)
        ++ "\n"
        ++ "  ]\n"
        ++ "}\n"

-- JSON is generated separately from test_cases.txt so run_tests.py consumes a
-- robust structured format instead of parsing human-readable text.
renderJsonTestCase :: ExportTestCase -> String
renderJsonTestCase testCase =
    "    {\n"
        ++ "      \"id\": "
        ++ show (testId testCase)
        ++ ",\n"
        ++ "      \"inputs\": "
        ++ renderJsonObject (inputAssignments testCase)
        ++ ",\n"
        ++ "      \"input_values\": "
        ++ renderJsonWordList (map snd (inputAssignments testCase))
        ++ ",\n"
        ++ "      \"input_events\": "
        ++ renderInputEvents (inputOccurrences testCase)
        ++ ",\n"
        ++ "      \"expected_branches\": "
        ++ renderJsonBranches (expectedBranches testCase)
        ++ ",\n"
        ++ "      \"expected_output\": "
        ++ renderJsonStringList (expectedOutput testCase)
        ++ ",\n"
        ++ "      \"path_condition\": "
        ++ jsonString (pathConditionText testCase)
        ++ ",\n"
        ++ "      \"notes\": "
        ++ jsonString (notes testCase)
        ++ "\n"
        ++ "    }"

-- Every occurrence carries both its solver identity and its source-level name.
renderInputEvents :: [(ReadEvent, String, Word32)] -> String
renderInputEvents events = "[" ++ intercalate "," (map one events) ++ "]"
  where
    one (event, name, value) =
        "{\"index\":"
            ++ show (readIndex event)
            ++ ",\"pc\":"
            ++ show (readPc event)
            ++ ",\"symbol\":"
            ++ jsonString ("val_" ++ show (readIndex event))
            ++ ",\"name\":"
            ++ jsonString name
            ++ ",\"value\":"
            ++ show value
            ++ "}"

renderJsonBranches :: [BranchEvent] -> String
renderJsonBranches events = "[" ++ intercalate "," (map one events) ++ "]"
  where
    one event =
        "{\"pc\":"
            ++ show (branchPc event)
            ++ ",\"taken\":"
            ++ (if branchTaken event then "true" else "false")
            ++ "}"

renderJsonObject :: [(String, Word32)] -> String
renderJsonObject fields =
    "{"
        ++ intercalate
            ", "
            [jsonString name ++ ": " ++ show value | (name, value) <- fields]
        ++ "}"

renderJsonWordList :: [Word32] -> String
renderJsonWordList values =
    "[" ++ intercalate ", " (map show values) ++ "]"

renderJsonStringList :: [String] -> String
renderJsonStringList values =
    "[" ++ intercalate ", " (map jsonString values) ++ "]"

jsonString :: String -> String
jsonString value =
    "\"" ++ concatMap escapeJson value ++ "\""

escapeJson :: Char -> String
escapeJson '"' = "\\\""
escapeJson '\\' = "\\\\"
escapeJson '\n' = "\\n"
escapeJson '\r' = "\\r"
escapeJson '\t' = "\\t"
escapeJson c = [c]

-- Each test runs in a separate Python process. A source-level step budget and
-- a parent timeout prevent a bad model or a changed program from hanging replay.
replayScript :: FilePath -> FilePath -> String
replayScript origFile jsonFile =
    unlines
        [ "#!/usr/bin/env python3"
        , "\"\"\"Replay generated tests in isolated processes with a time and step budget.\"\"\""
        , ""
        , "import argparse"
        , "import builtins"
        , "import contextlib"
        , "import io"
        , "import json"
        , "import runpy"
        , "import subprocess"
        , "import sys"
        , "from pathlib import Path"
        , ""
        , "ROOT = Path(__file__).resolve().parent"
        , "PROGRAM = ROOT / " ++ jsonString origFile
        , "TEST_CASES = ROOT / " ++ jsonString jsonFile
        , ""
        , ""
        , "def run_program(input_values, step_budget):"
        , "    \"\"\"Preserve the prototype's integer-valued input() convention.\"\"\""
        , "    values = iter(input_values)"
        , "    original_input = builtins.input"
        , "    previous_trace = sys.gettrace()"
        , "    steps = 0"
        , ""
        , "    def generated_input(prompt=\"\"):"
        , "        try:"
        , "            return int(next(values))"
        , "        except StopIteration as exc:"
        , "            raise RuntimeError(\"Program requested more inputs than the test provides\") from exc"
        , ""
        , "    def trace_source(frame, event, arg):"
        , "        nonlocal steps"
        , "        if event == \"line\" and frame.f_code.co_filename == str(PROGRAM):"
        , "            steps += 1"
        , "            if steps > step_budget:"
        , "                raise RuntimeError(\"Source replay exhausted its step budget\")"
        , "        return trace_source"
        , ""
        , "    stdout = io.StringIO()"
        , "    builtins.input = generated_input"
        , "    try:"
        , "        sys.settrace(trace_source)"
        , "        with contextlib.redirect_stdout(stdout):"
        , "            runpy.run_path(str(PROGRAM), run_name=\"__main__\")"
        , "    finally:"
        , "        sys.settrace(previous_trace)"
        , "        builtins.input = original_input"
        , "    if list(values):"
        , "        raise RuntimeError(\"Program did not consume all generated inputs\")"
        , "    return stdout.getvalue().splitlines()"
        , ""
        , ""
        , "def main():"
        , "    parser = argparse.ArgumentParser(description=__doc__)"
        , "    parser.add_argument(\"--timeout\", type=float, default=5.0)"
        , "    parser.add_argument(\"--step-budget\", type=int, default=100000)"
        , "    parser.add_argument(\"--worker\", action=\"store_true\", help=argparse.SUPPRESS)"
        , "    args = parser.parse_args()"
        , "    if args.timeout <= 0 or args.step_budget <= 0:"
        , "        parser.error(\"Timeout and step budget must be positive\")"
        , "    if args.worker:"
        , "        values = json.load(sys.stdin)"
        , "        print(json.dumps(run_program(values, args.step_budget)))"
        , "        return"
        , ""
        , "    tests = json.loads(TEST_CASES.read_text(encoding=\"utf-8\")).get(\"test_cases\", [])"
        , "    if not tests:"
        , "        print(\"No validated completed test cases are available.\")"
        , "        raise SystemExit(2)"
        , "    passed = 0"
        , "    for test in tests:"
        , "        try:"
        , "            process = subprocess.run("
        , "                [sys.executable, str(Path(__file__).resolve()), \"--worker\","
        , "                 \"--step-budget\", str(args.step_budget)],"
        , "                input=json.dumps(test.get(\"input_values\", [])),"
        , "                text=True, capture_output=True, timeout=args.timeout, check=True,"
        , "            )"
        , "            actual = json.loads(process.stdout)"
        , "            expected = test.get(\"expected_output\", [])"
        , "            # Preserve exact line order and values; ignore only trailing spaces."
        , "            ok = [str(x).rstrip() for x in actual] == [str(x).rstrip() for x in expected]"
        , "            print(f\"[{'PASS' if ok else 'FAIL'}] Test Case {test.get('id')}\")"
        , "            if not ok:"
        , "                print(f\"  inputs: {test.get('input_values', [])}\")"
        , "                print(f\"  expected: {expected}; actual: {actual}\")"
        , "            passed += int(ok)"
        , "        except subprocess.TimeoutExpired:"
        , "            print(f\"[FAIL] Test Case {test.get('id')}: replay timeout\")"
        , "        except (subprocess.CalledProcessError, ValueError) as exc:"
        , "            detail = exc.stderr.strip() if isinstance(exc, subprocess.CalledProcessError) else str(exc)"
        , "            print(f\"[FAIL] Test Case {test.get('id')}: {detail}\")"
        , "    print(f\"\\nSummary: {passed}/{len(tests)} tests passed\")"
        , "    raise SystemExit(0 if passed == len(tests) else 1)"
        , ""
        , ""
        , "if __name__ == \"__main__\":"
        , "    main()"
        ]

renderReadme :: FilePath -> String -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> String
renderReadme inputPath programName origFile stackFile txtFile jsonFile runnerFile readmeFile =
    unlines
        [ "# Generated tests for " ++ programName
        , ""
        , "This directory was generated by the Haskell symbolic execution tool for `" ++ takeBaseName inputPath ++ "`."
        , ""
        , "## Files"
        , ""
        , "- `" ++ origFile ++ "`: exact Python-like source text analyzed by the tool."
        , "- `" ++ stackFile ++ "`: compiled stack-language program with instruction addresses."
        , "- `" ++ txtFile ++ "`: human-readable generated test cases."
        , "- `" ++ jsonFile ++ "`: structured data consumed by `" ++ runnerFile ++ "`."
        , "- `" ++ runnerFile ++ "`: Python replay script that executes `" ++ origFile ++ "` for every generated test case."
        , "- `" ++ readmeFile ++ "`: this file."
        , ""
        , "## Running the replay"
        , ""
        , "From this directory, run:"
        , ""
        , "```sh"
        , "python3 " ++ runnerFile
        , "```"
        , ""
        , "The replay script patches `input()` so the Python-like program receives integer input values, matching the analyzed language subset."
        , "Printed lines are compared with the expected output after trimming trailing whitespace from each line."
        , ""
        , "## Test case format"
        , ""
        , "Each test case records input assignments, expected printed output, the rendered symbolic path condition, and a note identifying the validated completed symbolic path that produced it."
        ]

padLeft :: Int -> String -> String
padLeft width value =
    replicate (max 0 (width - length value)) ' ' ++ value

intercalate :: [a] -> [[a]] -> [a]
intercalate = List.intercalate
