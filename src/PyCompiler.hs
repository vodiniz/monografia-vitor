module PyCompiler where

import Prelude

import qualified Data.Map.Strict as M
import Data.Word (Word32)

import PySyntax
import Types

type Env = M.Map Name Word32

data CompileState = CompileState
    { env :: Env
    , nextAddr :: Word32
    }
    deriving (Eq, Show)

emptyCompileState :: CompileState
emptyCompileState =
    CompileState
        { env = M.empty
        , nextAddr = 0
        }

compileProgram :: PyProgram -> [Instr]
compileProgram (PyProgram stmts) =
    let (_, code) = compileStmtsAt emptyCompileState 0 stmts
     in code ++ [Done]

-- =========================================================
-- Variáveis
-- =========================================================

declareVar :: Name -> CompileState -> (CompileState, Word32)
declareVar name st =
    case M.lookup name (env st) of
        Just addr ->
            (st, addr)
        Nothing ->
            let addr = nextAddr st
                env' = M.insert name addr (env st)
                st' = st{env = env', nextAddr = addr + 1}
             in (st', addr)

lookupVar :: Name -> CompileState -> Word32
lookupVar name st =
    case M.lookup name (env st) of
        Just addr ->
            addr
        Nothing ->
            error ("Variable not defined: " ++ name)

-- =========================================================
-- Blocos de comandos
-- =========================================================

compileStmtsAt :: CompileState -> Int -> [PyStmt] -> (CompileState, [Instr])
compileStmtsAt st _ [] =
    (st, [])
compileStmtsAt st pc (stmt : stmts) =
    let (st1, code1) = compileStmtAt st pc stmt
        pc' = pc + length code1
        (st2, code2) = compileStmtsAt st1 pc' stmts
     in (st2, code1 ++ code2)

-- =========================================================
-- Comandos
-- =========================================================

compileStmtAt :: CompileState -> Int -> PyStmt -> (CompileState, [Instr])
compileStmtAt st _ (PyInput name) =
    let (st1, addr) = declareVar name st
     in ( st1
        ,
            [ Read
            , Push addr
            , Store
            ]
        )
compileStmtAt st _ (PyAssign name expr) =
    let (st1, exprCode) = compileExpr st expr
        (st2, addr) = declareVar name st1
     in ( st2
        , exprCode
            ++ [ Push addr
               , Store
               ]
        )
compileStmtAt st _ (PyPrint expr) =
    let (st1, exprCode) = compileExpr st expr
     in ( st1
        , exprCode ++ [Print]
        )
compileStmtAt st pc (PyIf cond thenStmts elseStmtsMaybe) =
    let
        elseStmts =
            case elseStmtsMaybe of
                Nothing -> []
                Just ss -> ss

        -- Código da condição.
        -- Ao final, deixa o resultado booleano no topo da pilha.
        (st1, condCode) =
            compileExpr st cond

        -- Estrutura gerada:
        --
        -- condCode
        -- Push thenAddr
        -- Swap
        -- JmpIf
        -- elseCode
        -- Push endAddr
        -- Push 1
        -- JmpIf
        -- thenCode
        --
        -- Se cond for verdadeira, pula para thenAddr.
        -- Se cond for falsa, cai no elseCode.
        -- Depois do elseCode, pula incondicionalmente para endAddr.

        pcAfterCondJump =
            pc + length condCode + 3

        (st2, elseCode) =
            compileStmtsAt st1 pcAfterCondJump elseStmts

        pcAfterElse =
            pcAfterCondJump + length elseCode

        pcAfterJumpOverThen =
            pcAfterElse + 3

        (st3, thenCode) =
            compileStmtsAt st2 pcAfterJumpOverThen thenStmts

        thenAddr =
            fromIntegral pcAfterJumpOverThen

        endAddr =
            fromIntegral (pcAfterJumpOverThen + length thenCode)

        code =
            condCode
                ++ [ Push thenAddr
                   , Swap
                   , JmpIf
                   ]
                ++ elseCode
                ++ [ Push endAddr
                   , Push 1
                   , JmpIf
                   ]
                ++ thenCode
     in
        (st3, code)

-- =========================================================
-- Expressões
-- =========================================================

compileExpr :: CompileState -> PyExp -> (CompileState, [Instr])
compileExpr st (PyVal n) =
    (st, [Push n])
compileExpr st (PyVar name) =
    let addr = lookupVar name st
     in (st, [Push addr, Load])
compileExpr st (PyAdd e1 e2) =
    let (st1, c1) = compileExpr st e1
        (st2, c2) = compileExpr st1 e2
     in (st2, c1 ++ c2 ++ [Add])
compileExpr st (PyAnd e1 e2) =
    let (st1, c1) = compileExpr st e1
        (st2, c2) = compileExpr st1 e2
     in (st2, c1 ++ c2 ++ [And])
compileExpr st (PyOr e1 e2) =
    let (st1, c1) = compileExpr st e1
        (st2, c2) = compileExpr st1 e2
     in (st2, c1 ++ c2 ++ [Or])
compileExpr st (PyNot e) =
    let (st1, c) = compileExpr st e
     in (st1, c ++ [Not])
compileExpr st (PyEq e1 e2) =
    let (st1, c1) = compileExpr st e1
        (st2, c2) = compileExpr st1 e2
     in (st2, c1 ++ c2 ++ [Eq])
compileExpr st (PyLt e1 e2) =
    let (st1, c1) = compileExpr st e1
        (st2, c2) = compileExpr st1 e2
     in (st2, c2 ++ c1 ++ [Lt])
compileExpr st (PyGt e1 e2) =
    let (st1, c1) = compileExpr st e1
        (st2, c2) = compileExpr st1 e2
     in (st2, c1 ++ c2 ++ [Lt])
