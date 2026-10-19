-- | Bounded concrete evaluation using Python-style, unbounded integer arithmetic.
module PyEval (evalPyProgram, evalPyProgramWithBudget) where

import qualified Data.Map.Strict as M
import Data.Word (Word32)
import PyNormalize (normalizeProgram)
import PySyntax
import Prelude

type Environment = M.Map Name Integer
type EvalResult = (Int, Environment, [Integer], [String])

evalPyProgram :: PyProgram -> [Word32] -> Either String [String]
evalPyProgram = evalPyProgramWithBudget 100000

evalPyProgramWithBudget :: Int -> PyProgram -> [Word32] -> Either String [String]
evalPyProgramWithBudget budget ast inputs = do
    let PyProgram stmts = normalizeProgram ast
    (_, _, remaining, outputs) <- evalStmts budget M.empty (map toInteger inputs) [] stmts
    if null remaining then Right outputs else Left "The source did not consume all generated inputs."

-- A single remaining budget is threaded through all blocks and all iterations.
-- Resetting it when entering a body would allow an infinite loop to run forever.
evalStmts :: Int -> Environment -> [Integer] -> [String] -> [PyStmt] -> Either String EvalResult
evalStmts fuel env inputs outputs [] = Right (fuel, env, inputs, outputs)
evalStmts fuel env inputs outputs (stmt : rest) = do
    (fuel', env', inputs', outputs') <- evalStmt fuel env inputs outputs stmt
    evalStmts fuel' env' inputs' outputs' rest

evalStmt :: Int -> Environment -> [Integer] -> [String] -> PyStmt -> Either String EvalResult
evalStmt fuel _ _ _ _ | fuel <= 0 = Left "Concrete source evaluation exhausted its step budget."
evalStmt fuel env (value : inputs) outputs (PyInput name) =
    Right (fuel - 1, M.insert name value env, inputs, outputs)
evalStmt _ _ [] _ (PyInput name) = Left ("Missing input value for " ++ name ++ ".")
evalStmt fuel env inputs outputs (PyAssign name expr) = do
    value <- evalExpr env expr
    Right (fuel - 1, M.insert name value env, inputs, outputs)
evalStmt fuel env inputs outputs (PyPrint expr) = do
    value <- evalExpr env expr
    Right (fuel - 1, env, inputs, outputs ++ [show value])
evalStmt fuel env inputs outputs (PyIf cond yes no) = do
    value <- evalExpr env cond
    evalStmts (fuel - 1) env inputs outputs (if value /= 0 then yes else maybe [] id no)
evalStmt fuel env inputs outputs loop@(PyWhile cond body) = do
    value <- evalExpr env cond
    if value == 0
        then Right (fuel - 1, env, inputs, outputs)
        else do
            (fuel', env', inputs', outputs') <- evalStmts (fuel - 1) env inputs outputs body
            evalStmt fuel' env' inputs' outputs' loop
evalStmt _ _ _ _ (PyForRange _ _ _ _) = Left "Range loops must be normalized before evaluation."

evalExpr :: Environment -> PyExp -> Either String Integer
evalExpr _ (PyVal value) = Right (toInteger value)
evalExpr env (PyVar name) = maybe (Left ("Variable not defined: " ++ name ++ ".")) Right (M.lookup name env)
evalExpr env (PyAdd l r) = (+) <$> evalExpr env l <*> evalExpr env r
-- The supported language uses boolean-valued and/or, as the original VM does.
-- Full Python operand-returning and short-circuit semantics remain out of scope.
evalExpr env (PyAnd l r) = boolToInteger <$> ((&&) <$> evalBool env l <*> evalBool env r)
evalExpr env (PyOr l r) = boolToInteger <$> ((||) <$> evalBool env l <*> evalBool env r)
evalExpr env (PyNot expr) = boolToInteger . not <$> evalBool env expr
evalExpr env (PyLt l r) = boolToInteger <$> ((<) <$> evalExpr env l <*> evalExpr env r)
evalExpr env (PyGt l r) = boolToInteger <$> ((>) <$> evalExpr env l <*> evalExpr env r)
evalExpr env (PyEq l r) = boolToInteger <$> ((==) <$> evalExpr env l <*> evalExpr env r)

evalBool :: Environment -> PyExp -> Either String Bool
evalBool env expr = (/= 0) <$> evalExpr env expr

boolToInteger :: Bool -> Integer
boolToInteger True = 1
boolToInteger False = 0
