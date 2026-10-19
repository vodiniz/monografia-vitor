-- | Lower source-level range loops to the single loop form understood by the VM.
module PyNormalize (normalizeProgram) where

import qualified Data.Set as Set
import PySyntax
import Prelude

normalizeProgram :: PyProgram -> PyProgram
normalizeProgram (PyProgram stmts) =
    let (_, normalized) = normalizeStmts (Set.unions (map stmtNames stmts)) stmts
     in PyProgram normalized

-- Thread the used-name set through every block, including both branches of if.
-- Temporaries are therefore unique throughout the program, not just one loop.
normalizeStmts :: Set.Set Name -> [PyStmt] -> (Set.Set Name, [PyStmt])
normalizeStmts used [] = (used, [])
normalizeStmts used (stmt : rest) =
    let (used1, first) = normalizeStmt used stmt
        (used2, remaining) = normalizeStmts used1 rest
     in (used2, first ++ remaining)

normalizeStmt :: Set.Set Name -> PyStmt -> (Set.Set Name, [PyStmt])
normalizeStmt used (PyIf cond yes no) =
    let (used1, yes') = normalizeStmts used yes
        (used2, no') = normalizeStmts used1 (maybe [] id no)
     in (used2, [PyIf cond yes' (fmap (const no') no)])
normalizeStmt used (PyWhile cond body) =
    let (used1, body') = normalizeStmts used body
     in (used1, [PyWhile cond body'])
normalizeStmt used (PyForRange name start stop body) =
    let (used1, startName) = freshName used "start"
        (used2, stopName) = freshName used1 "stop"
        (used3, indexName) = freshName used2 "index"
        (used4, body') = normalizeStmts used3 body
        -- Evaluate both bounds once, in source order. Keep the iterator separate
        -- from the visible variable: assigning to that variable inside the body
        -- must not change range progression. An empty range never assigns it.
        lowered =
            [ PyAssign startName start
            , PyAssign stopName stop
            , PyAssign indexName (PyVar startName)
            , PyWhile
                (PyLt (PyVar indexName) (PyVar stopName))
                ( PyAssign name (PyVar indexName)
                    : body'
                    ++ [PyAssign indexName (PyAdd (PyVar indexName) (PyVal 1))]
                )
            ]
     in (used4, lowered)
normalizeStmt used stmt = (used, [stmt])

-- '$' cannot occur in a parsed identifier. Still check every generated name
-- against the AST so callers that construct ASTs directly remain safe.
freshName :: Set.Set Name -> String -> (Set.Set Name, Name)
freshName used role =
    let candidates = ["$range_" ++ role ++ "_" ++ show n | n <- [0 :: Int ..]]
        name = head (filter (`Set.notMember` used) candidates)
     in (Set.insert name used, name)

stmtNames :: PyStmt -> Set.Set Name
stmtNames (PyInput name) = Set.singleton name
stmtNames (PyPrint expr) = exprNames expr
stmtNames (PyAssign name expr) = Set.insert name (exprNames expr)
stmtNames (PyIf cond yes no) =
    Set.unions (exprNames cond : map stmtNames (yes ++ maybe [] id no))
stmtNames (PyWhile cond body) = Set.unions (exprNames cond : map stmtNames body)
stmtNames (PyForRange name start stop body) =
    Set.insert name (Set.unions (exprNames start : exprNames stop : map stmtNames body))

exprNames :: PyExp -> Set.Set Name
exprNames (PyVar name) = Set.singleton name
exprNames (PyVal _) = Set.empty
exprNames (PyNot expr) = exprNames expr
exprNames (PyAdd l r) = pairNames l r
exprNames (PyAnd l r) = pairNames l r
exprNames (PyOr l r) = pairNames l r
exprNames (PyLt l r) = pairNames l r
exprNames (PyGt l r) = pairNames l r
exprNames (PyEq l r) = pairNames l r

pairNames :: PyExp -> PyExp -> Set.Set Name
pairNames l r = Set.union (exprNames l) (exprNames r)
