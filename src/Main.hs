module Main where

import ArtifactExport
import qualified Data.Tree as T
import qualified Foundation as F
import Programs
import PyCompiler
import PyNormalize
import PyParser
import SMT
import Symbolic
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)
import Types
import Prelude

-- Preserve both previous invocation styles: no arguments runs the inline demo;
-- a single path analyses that file. New options configure independent budgets.
main :: IO ()
main = do
    args <- getArgs
    case parseOptions args of
        Left err -> die (err ++ "\nUsage: cabal run symbolic -- [input-path] [--loop-k N] [--max-depth N] [--show-tree]")
        Right (config, path, showTree) -> do
            let inputPath = maybe "inline-example.py" id path
            source <- maybe (return inlineExampleSource) readFile path
            runAnalysisWith config showTree inputPath source

parseOptions :: [String] -> Either String (AnalysisConfig, Maybe FilePath, Bool)
parseOptions = go defaultAnalysisConfig Nothing False
  where
    go config path showTree [] = Right (config, path, showTree)
    go config path showTree ("--loop-k" : raw : rest) = do
        value <- nonnegative "loop-k" raw
        go (config{loopK = value}) path showTree rest
    go config path showTree ("--max-depth" : raw : rest) = do
        value <- nonnegative "max-depth" raw
        go (config{maxDepth = value}) path showTree rest
    go config path _ ("--show-tree" : rest) = go config path True rest
    go _ _ _ ["--loop-k"] = Left "--loop-k requires an integer."
    go _ _ _ ["--max-depth"] = Left "--max-depth requires an integer."
    go config Nothing showTree (arg : rest)
        | take 2 arg /= "--" = go config (Just arg) showTree rest
    go _ _ _ (arg : _) = Left ("Unexpected argument: " ++ arg)
    nonnegative name raw = case readMaybe raw of
        Just n | n >= 0 -> Right n
        _ -> Left (name ++ " must be a non-negative Int.")

inlineExampleSource :: String
inlineExampleSource =
    unlines
        [ "x = input()"
        , "y = input()"
        , "z = x + y"
        , ""
        , "if z > 15:"
        , "    print(z)"
        , "else:"
        , "    print(0)"
        ]

runAnalysis :: FilePath -> String -> IO ()
runAnalysis = runAnalysisWith defaultAnalysisConfig False

runAnalysisWith :: AnalysisConfig -> Bool -> FilePath -> String -> IO ()
runAnalysisWith config showTree inputPath source =
    case pyParser source of
        Left err -> die err
        Right sourceAst -> do
            let ast = normalizeProgram sourceAst
                (instrs, info) = compileProgramWithInfo ast
                prog = listToProgram instrs
                traces = symRunWith config info prog defaultSymState
            putStrLn ("Analysis limits: " ++ show config)
            putStrLn "Generated instructions:"
            putStrLn (renderStackProgram instrs)
            -- Trees may be large even for small loop bounds. Keep inspection
            -- available explicitly without flooding the default CLI output.
            if showTree then putStrLn (T.drawTree (fmap show traces)) else return ()
            solved <- solveSym traces
            if showTree then putStrLn (T.drawTree (fmap (F.toList . renderSolvedState) solved)) else return ()
            outputDir <- exportArtifactsWith config info inputPath source ast instrs solved
            putStrLn ("Generated test artifacts saved to: " ++ outputDir)
