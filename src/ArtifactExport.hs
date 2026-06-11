module ArtifactExport
    ( exportArtifacts
    ) where

import Prelude

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.SBV.Dynamic as S
import qualified Data.Tree as T
import Data.Word (Word32)
import qualified Foundation as F
import System.Directory (createDirectoryIfMissing)
import System.FilePath
    ( dropExtension
    , takeBaseName
    , takeFileName
    , (</>)
    )
import Text.Read (readMaybe)

import PySyntax
import SMT
import Symbolic
import Types
import Util

data ExportTestCase = ExportTestCase
    { testId :: Int
    , inputAssignments :: [(String, Word32)]
    -- Expected output is stored as strings because the replay script compares
    -- printed Python lines, not internal Word32 stack values.
    , expectedOutput :: [String]
    , pathConditionText :: String
    , notes :: String
    }
    deriving (Eq, Show)

exportArtifacts ::
    FilePath ->
    String ->
    PyProgram ->
    [Instr] ->
    T.Tree SolvedState ->
    IO FilePath
exportArtifacts inputPath source ast instrs solvedTraces = do
    let programName = programFolderName inputPath
        outputDir = "generated-tests" </> programName
        inputNames = collectInputs ast
        testCases = buildTestCases ast inputNames solvedTraces

    -- Keep every artifact for one analyzed program in a deterministic folder so
    -- repeated runs are easy to inspect and compare.
    createDirectoryIfMissing True outputDir
    writeFile (outputDir </> "original.py") source
    writeFile (outputDir </> "stack_program.txt") (renderStackProgram instrs)
    writeFile (outputDir </> "test_cases.txt") (renderTestCases testCases)
    writeFile (outputDir </> "test_cases.json") (renderTestCasesJson testCases)
    writeFile (outputDir </> "run_tests.py") replayScript
    writeFile (outputDir </> "README.md") (renderReadme inputPath programName)
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

collectInputs :: PyProgram -> [String]
collectInputs (PyProgram stmts) =
    concatMap collectStmtInputs stmts

-- Input variables are collected from the AST so exported assignments use source
-- names like x and y instead of only solver names like val_0 and val_1.
collectStmtInputs :: PyStmt -> [String]
collectStmtInputs (PyInput name) = [name]
collectStmtInputs (PyPrint _) = []
collectStmtInputs (PyAssign _ _) = []
collectStmtInputs (PyIf _ thenStmts elseStmts) =
    concatMap collectStmtInputs thenStmts
        ++ maybe [] (concatMap collectStmtInputs) elseStmts

buildTestCases :: PyProgram -> [String] -> T.Tree SolvedState -> [ExportTestCase]
buildTestCases ast inputNames solvedTraces =
    zipWith mkTestCase [1 ..] satisfiableLeaves
  where
    -- Each satisfiable symbolic leaf represents one feasible execution path.
    -- Unsatisfiable leaves are solver artifacts and should not become tests.
    satisfiableLeaves =
        filter isSatisfiableSolvedState (leaves solvedTraces)

    mkTestCase n solved@(SolvedState state@(_, inputCount, _, _, cs) smtResult) =
        let model = modelAssignments smtResult
            inputValues = concreteInputs inputNames inputCount model
            -- Expected output must match original.py. We evaluate the parsed
            -- Python-like AST instead of replaying the stack program because the
            -- compiler uses Word32 stack values internally, while Python prints
            -- unbounded integers from input().
            expected =
                either
                    (\err -> ["<error: " ++ err ++ ">"])
                    id
                    (evalPyProgram ast (map snd inputValues))
         in ExportTestCase
                { testId = n
                , inputAssignments = inputValues
                , expectedOutput = expected
                , pathConditionText = F.toList (renderSym (foldr SAnd (SCon 1) cs))
                , notes = "Derived from solved symbolic leaf at PC " ++ showPc state solved
                }

showPc :: SymState -> SolvedState -> String
showPc (pc, _, _, _, _) _ = show pc

leaves :: T.Tree a -> [a]
leaves (T.Node x []) = [x]
leaves (T.Node _ children) = concatMap leaves children

isSatisfiableSolvedState :: SolvedState -> Bool
isSatisfiableSolvedState (SolvedState _ (S.Satisfiable _ _)) = True
isSatisfiableSolvedState _ = False

modelAssignments :: S.SMTResult -> M.Map String Word32
modelAssignments result@(S.Satisfiable _ _) =
    M.mapMaybe parseModelWord (M.mapKeys F.toList (S.getModelDictionary result))
modelAssignments _ =
    M.empty

parseModelWord :: Show a => a -> Maybe Word32
parseModelWord value =
    readMaybe (takeWhile Char.isDigit (show value))

-- The solver model is keyed by val_N. This maps those values back to source
-- input names in declaration order, falling back to val_N for defensive output.
concreteInputs :: [String] -> Int -> M.Map String Word32 -> [(String, Word32)]
concreteInputs inputNames inputCount model =
    [ (inputName i, M.findWithDefault 0 (F.toList (valName i)) model)
        | i <- [0 .. inputCount - 1]
    ]
  where
    inputName i =
        case drop i inputNames of
            name : _ -> name
            [] -> F.toList (valName i)

evalPyProgram :: PyProgram -> [Word32] -> Either String [String]
evalPyProgram (PyProgram stmts) inputs = do
    (_, _, outputs) <- evalStmts M.empty (map toInteger inputs) [] stmts
    return outputs

-- Small interpreter for the parsed Python-like subset. It exists only to
-- compute replay expectations for original.py, keeping export independent from
-- the interactive Concrete runner.
evalStmts ::
    M.Map String Integer ->
    [Integer] ->
    [String] ->
    [PyStmt] ->
    Either String (M.Map String Integer, [Integer], [String])
evalStmts env inputs outputs [] =
    Right (env, inputs, outputs)
evalStmts env inputs outputs (stmt : stmts) = do
    (env', inputs', outputs') <- evalStmt env inputs outputs stmt
    evalStmts env' inputs' outputs' stmts

evalStmt ::
    M.Map String Integer ->
    [Integer] ->
    [String] ->
    PyStmt ->
    Either String (M.Map String Integer, [Integer], [String])
evalStmt env (value : inputs) outputs (PyInput name) =
    Right (M.insert name value env, inputs, outputs)
evalStmt _ [] _ (PyInput name) =
    Left ("Missing input value for " ++ name ++ ".")
evalStmt env inputs outputs (PyAssign name expr) = do
    value <- evalExpr env expr
    Right (M.insert name value env, inputs, outputs)
evalStmt env inputs outputs (PyPrint expr) = do
    value <- evalExpr env expr
    Right (env, inputs, outputs ++ [show value])
evalStmt env inputs outputs (PyIf cond thenStmts elseStmts) = do
    value <- evalExpr env cond
    if value /= 0
        then evalStmts env inputs outputs thenStmts
        else evalStmts env inputs outputs (maybe [] id elseStmts)

evalExpr :: M.Map String Integer -> PyExp -> Either String Integer
evalExpr _ (PyVal value) =
    Right (toInteger value)
evalExpr env (PyVar name) =
    case M.lookup name env of
        Just value -> Right value
        Nothing -> Left ("Variable not defined: " ++ name ++ ".")
evalExpr env (PyAdd l r) =
    (+) <$> evalExpr env l <*> evalExpr env r
evalExpr env (PyAnd l r) =
    boolToInteger <$> ((&&) <$> evalBool env l <*> evalBool env r)
evalExpr env (PyOr l r) =
    boolToInteger <$> ((||) <$> evalBool env l <*> evalBool env r)
evalExpr env (PyNot expr) =
    boolToInteger . not <$> evalBool env expr
evalExpr env (PyLt l r) =
    boolToInteger <$> ((<) <$> evalExpr env l <*> evalExpr env r)
evalExpr env (PyGt l r) =
    boolToInteger <$> ((>) <$> evalExpr env l <*> evalExpr env r)
evalExpr env (PyEq l r) =
    boolToInteger <$> ((==) <$> evalExpr env l <*> evalExpr env r)

evalBool :: M.Map String Integer -> PyExp -> Either String Bool
evalBool env expr =
    (/= 0) <$> evalExpr env expr

boolToInteger :: Bool -> Integer
boolToInteger True = 1
boolToInteger False = 0

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

replayScript :: String
replayScript =
    unlines
        [ "#!/usr/bin/env python3"
        , "\"\"\"Replay generated symbolic-execution test cases.\"\"\""
        , ""
        , "import builtins"
        , "import contextlib"
        , "import io"
        , "import json"
        , "import runpy"
        , "from pathlib import Path"
        , ""
        , ""
        , "ROOT = Path(__file__).resolve().parent"
        , "PROGRAM = ROOT / \"original.py\""
        , "TEST_CASES = ROOT / \"test_cases.json\""
        , ""
        , ""
        , "def normalize(lines):"
        , "    \"\"\"Compare printed output after trimming trailing whitespace."
        , ""
        , "    This keeps comparisons exact for line order and values, while avoiding"
        , "    failures caused only by trailing spaces in print output."
        , "    \"\"\""
        , "    return [str(line).rstrip() for line in lines]"
        , ""
        , ""
        , "def run_program(input_values):"
        , "    values = iter(input_values)"
        , "    original_input = builtins.input"
        , ""
        , "    def generated_input(prompt=\"\"):"
        , "        try:"
        , "            return int(next(values))"
        , "        except StopIteration as exc:"
        , "            raise RuntimeError(\"Program requested more input values than the test provides\") from exc"
        , ""
        , "    stdout = io.StringIO()"
        , "    builtins.input = generated_input"
        , "    try:"
        , "        with contextlib.redirect_stdout(stdout):"
        , "            runpy.run_path(str(PROGRAM), run_name=\"__main__\")"
        , "    finally:"
        , "        builtins.input = original_input"
        , ""
        , "    return stdout.getvalue().splitlines()"
        , ""
        , ""
        , "def main():"
        , "    data = json.loads(TEST_CASES.read_text(encoding=\"utf-8\"))"
        , "    tests = data.get(\"test_cases\", [])"
        , "    passed = 0"
        , ""
        , "    for test in tests:"
        , "        actual = run_program(test.get(\"input_values\", []))"
        , "        expected = test.get(\"expected_output\", [])"
        , "        ok = normalize(actual) == normalize(expected)"
        , "        status = \"PASS\" if ok else \"FAIL\""
        , "        print(f\"[{status}] Test Case {test.get('id')}\")"
        , "        if not ok:"
        , "            print(f\"  inputs:   {test.get('inputs', {})}\")"
        , "            print(f\"  expected: {expected}\")"
        , "            print(f\"  actual:   {actual}\")"
        , "            print(f\"  path:     {test.get('path_condition', '')}\")"
        , "        passed += 1 if ok else 0"
        , ""
        , "    print(f\"\\nSummary: {passed}/{len(tests)} tests passed\")"
        , "    raise SystemExit(0 if passed == len(tests) else 1)"
        , ""
        , ""
        , "if __name__ == \"__main__\":"
        , "    main()"
        ]

renderReadme :: FilePath -> String -> String
renderReadme inputPath programName =
    unlines
        [ "# Generated tests for " ++ programName
        , ""
        , "This directory was generated by the Haskell symbolic execution tool for `" ++ takeBaseName inputPath ++ "`."
        , ""
        , "## Files"
        , ""
        , "- `original.py`: exact Python-like source text analyzed by the tool."
        , "- `stack_program.txt`: compiled stack-language program with instruction addresses."
        , "- `test_cases.txt`: human-readable generated test cases."
        , "- `test_cases.json`: structured data consumed by `run_tests.py`."
        , "- `run_tests.py`: Python replay script that executes `original.py` for every generated test case."
        , "- `README.md`: this file."
        , ""
        , "## Running the replay"
        , ""
        , "From this directory, run:"
        , ""
        , "```sh"
        , "python3 run_tests.py"
        , "```"
        , ""
        , "The replay script patches `input()` so the Python-like program receives integer input values, matching the analyzed language subset."
        , "Printed lines are compared with the expected output after trimming trailing whitespace from each line."
        , ""
        , "## Test case format"
        , ""
        , "Each test case records input assignments, expected printed output, the rendered symbolic path condition, and a note identifying the solved symbolic leaf that produced it."
        ]

padLeft :: Int -> String -> String
padLeft width value =
    replicate (max 0 (width - length value)) ' ' ++ value

intercalate :: [a] -> [[a]] -> [a]
intercalate = List.intercalate
