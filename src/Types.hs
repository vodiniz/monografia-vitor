module Types where

import Foundation

import qualified Data.Map.Strict as M
import qualified Data.Tree as T
import qualified Prelude as P

data Instr
    = Add
    | JmpIf
    | And
    | Or
    | Not
    | Lt
    | Eq
    | Push Word32
    | Store
    | Load
    | Pop
    | Read
    | Print
    | Swap
    | Dup
    | Over
    | RotL
    | Done
    deriving (Eq, Show)

-- | A program is a list of instructions.
type Prog = Array Instr

-- | Memory is a list of 32 bit words.
type Mem = M.Map Word32 Word32

-- | State: (program counter, memory, stack)
type State = (Int, Mem, [Word32])

data Sym
    = SAdd Sym Sym
    | SEq Sym Sym
    | SNot Sym
    | SOr Sym Sym
    | SCon Word32
    | SAnd Sym Sym
    | SLt Sym Sym
    | SAny Int
    deriving (Show, Eq, Ord)

-- Metadata describes canonical loops emitted by PyCompiler. It does not add
-- instructions or change the concrete stack-machine semantics.
data LoopInfo = LoopInfo
    { loopHeader :: Int
    , loopGuard :: Int
    , loopBody :: Int
    , loopBackEdge :: Int
    , loopExit :: Int
    }
    deriving (Eq, Show)

data ProgramInfo = ProgramInfo
    { programLoops :: M.Map Int LoopInfo
    , programReads :: M.Map Int P.String
    }
    deriving (Eq, Show)

emptyProgramInfo :: ProgramInfo
emptyProgramInfo = ProgramInfo M.empty M.empty

data AnalysisConfig = AnalysisConfig
    { loopK :: Int
    , maxDepth :: Int
    }
    deriving (Eq, Show)

defaultAnalysisConfig :: AnalysisConfig
defaultAnalysisConfig = AnalysisConfig 2 1000

-- Occurrences, rather than variable names, identify dynamically repeated input.
data ReadEvent = ReadEvent
    { readIndex :: Int
    , readPc :: Int
    }
    deriving (Eq, Show)

data BranchEvent = BranchEvent
    { branchPc :: Int
    , branchTaken :: Bool
    }
    deriving (Eq, Show)

data SymContext = SymContext
    { loopCounts :: M.Map Int Int
    , readEvents :: [ReadEvent]
    , branchEvents :: [BranchEvent]
    }
    deriving (Eq, Show)

-- Keep the original five components in their original order. The sixth carries
-- path-local metadata; immutable states automatically isolate sibling branches.
type SymState = (Int, Int, M.Map Word32 Sym, [Sym], [Sym], SymContext)

data StopReason
    = Running
    | Completed
    | CutByLoop Int
    | CutByDepth
    | ExecutionError P.String
    deriving (Eq, Show)

data TraceNode = TraceNode
    { traceState :: SymState
    , stopReason :: StopReason
    }
    deriving (Eq, Show)

type Trace = T.Tree TraceNode
