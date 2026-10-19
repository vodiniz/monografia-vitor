module PyCompiler where

import Prelude

import qualified Data.Map.Strict as M
import Data.Word (Word32)

import PyNormalize (normalizeProgram)
import PySyntax
import Types

type Env = M.Map Name Word32

data CompileState = CompileState
    { env :: Env
    , nextAddr :: Word32
    , compiledLoops :: M.Map Int LoopInfo
    , compiledReads :: M.Map Int Name
    }
    deriving (Eq, Show)

emptyCompileState :: CompileState
emptyCompileState =
    CompileState
        { env = M.empty
        , nextAddr = 0
        , compiledLoops = M.empty
        , compiledReads = M.empty
        }

compileProgram :: PyProgram -> [Instr]
compileProgram = fst . compileProgramWithInfo

-- Preserve the old entry point while offering source metadata to the bounded
-- executor and exporter. Normalization is idempotent, so direct callers are safe.
compileProgramWithInfo :: PyProgram -> ([Instr], ProgramInfo)
compileProgramWithInfo ast =
    let PyProgram stmts = normalizeProgram ast
        (st, code) = compileStmtsAt emptyCompileState 0 stmts
     in (code ++ [Done], ProgramInfo (compiledLoops st) (compiledReads st))

-- =========================================================
-- Variables
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
-- Statement blocks
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
-- Statements
-- =========================================================

compileStmtAt :: CompileState -> Int -> PyStmt -> (CompileState, [Instr])
compileStmtAt st pc (PyInput name) =
    let (st1, addr) = declareVar name st
     in ( st1{compiledReads = M.insert pc name (compiledReads st1)}
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

        -- Compile the condition.
        -- The resulting boolean value remains at the top of the stack.
        (st1, condCode) =
            compileExpr st cond

        -- Generated layout:
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
        -- A true condition jumps to thenAddr.
        -- A false condition falls through to elseCode.
        -- After elseCode, jump unconditionally to endAddr.

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
-- Compile a while without adding VM instructions. The guard exits when the
-- original condition is false; the final constant-true jump revisits the header.
-- Compile nested blocks at their final absolute PCs before deriving loopExit.
compileStmtAt st pc (PyWhile cond body) =
    let (st1, condCode) = compileExpr st cond
        guardPc = pc + length condCode + 3
        bodyPc = guardPc + 1
        (st2, bodyCode) = compileStmtsAt st1 bodyPc body
        backPc = bodyPc + length bodyCode + 2
        endPc = backPc + 1
        info = LoopInfo pc guardPc bodyPc backPc endPc
        st3 = st2{compiledLoops = M.insert pc info (compiledLoops st2)}
        code =
            condCode
                ++ [Push (fromIntegral endPc), Swap, Not, JmpIf]
                ++ bodyCode
                ++ [Push (fromIntegral pc), Push 1, JmpIf]
     in (st3, code)
compileStmtAt _ _ (PyForRange _ _ _ _) =
    error "Range loops must be normalized before compilation."

-- =========================================================
-- Expressions
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
