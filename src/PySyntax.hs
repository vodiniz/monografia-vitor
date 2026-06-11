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
