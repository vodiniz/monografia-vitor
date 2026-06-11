module Main where

import Prelude

import qualified Data.Tree as T
import qualified Foundation as F
import System.Environment (getArgs)

import ArtifactExport
import Programs
import PyCompiler
import PyParser
import SMT
import Symbolic

main :: IO ()
main = do
    args <- getArgs
    case args of
        -- Keep the previous demo behavior available when no file is passed.
        [] ->
            runAnalysis "inline-example.py" inlineExampleSource
        [inputPath] -> do
            source <- readFile inputPath
            runAnalysis inputPath source
        _ ->
            putStrLn "Usage: cabal run symbolic -- <input-path>"

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
runAnalysis inputPath source =
    case pyParser source of
        Left err ->
            putStrLn err
        Right ast -> do
            putStrLn "Source:"
            putStrLn source

            putStrLn "AST:"
            print ast

            let instrs = compileProgram ast

            putStrLn "\nGenerated instructions:"
            print instrs

            let prog = listToProgram instrs

            putStrLn "\nSymbolic execution tree:"
            -- Reuse the existing symbolic execution pipeline; export happens after
            -- solving so the artifact layer can inspect concrete SMT models.
            let traces = symRun 50 prog defaultSymState

            putStrLn $
                T.drawTree $
                    fmap
                        ( show
                            . \(pc, _, _, st, cs) ->
                                ( pc
                                , renderSym <$> st
                                , renderSym <$> cs
                                )
                        )
                        traces

            putStrLn "\nSolved symbolic execution tree:"
            solvedTraces <- solveSym traces

            putStrLn $
                T.drawTree $
                    fmap (F.toList . renderSolvedState) solvedTraces

            outputDir <- exportArtifacts inputPath source ast instrs solvedTraces
            putStrLn ("\nGenerated test artifacts saved to: " ++ outputDir)
