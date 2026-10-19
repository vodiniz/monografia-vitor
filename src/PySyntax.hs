module PySyntax where

import Data.Word (Word32)
import Prelude

type Name = String

data PyProgram
    = PyProgram [PyStmt]
    deriving (Eq, Ord, Show)

data PyStmt
    = PyInput Name
    | PyPrint PyExp
    | PyAssign Name PyExp
    | PyIf PyExp [PyStmt] (Maybe [PyStmt])
    | -- Store the condition and the statements executed by a while loop.
      PyWhile PyExp [PyStmt]
    | -- Store the target variable, range bounds, and loop body.
      PyForRange Name PyExp PyExp [PyStmt]
    deriving (Eq, Ord, Show)

data PyExp
    = PyVar Name
    | PyVal Word32
    | PyAdd PyExp PyExp
    | PyAnd PyExp PyExp
    | PyOr PyExp PyExp
    | PyNot PyExp
    | PyLt PyExp PyExp
    | PyGt PyExp PyExp
    | PyEq PyExp PyExp
    deriving (Eq, Ord, Show)
