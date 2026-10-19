{- | Replay compiled instructions with unbounded integers before accepting a test.
This checks actual branch choices, not just final output. In particular, a
Word32 solver model whose path relies on overflow must not silently pass.
-}
module ConcreteReplay (ReplayResult (..), replayConcrete) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Word (Word32)
import Types
import Prelude

-- Events are returned in execution order, matching the source input stream.
data ReplayResult = ReplayResult
    { replayOutput :: [String]
    , replayBranches :: [BranchEvent]
    }
    deriving (Eq, Show)

replayConcrete :: Int -> [Instr] -> [Word32] -> Either String ReplayResult
replayConcrete budget instructions inputs = walk budget 0 M.empty [] (map toInteger inputs) [] []
  where
    program = IM.fromList (zip [0 ..] instructions)
    walk fuel pc mem stack remaining outputs branches =
        case IM.lookup pc program of
            Nothing -> Left "Concrete replay reached an invalid program counter."
            Just Done
                | null remaining -> Right (ReplayResult (reverse outputs) (reverse branches))
                | otherwise -> Left "Concrete replay did not consume all generated inputs."
            Just _ | fuel <= 0 -> Left "Concrete replay exhausted its instruction budget."
            Just instruction ->
                let next = walk (fuel - 1) (pc + 1)
                    binary op = case stack of
                        l : r : rest -> next mem (op l r : rest) remaining outputs branches
                        _ -> Left "Concrete replay: binary instruction requires two operands."
                 in case (instruction, stack) of
                        (Push value, _) -> next mem (toInteger value : stack) remaining outputs branches
                        (Read, _) -> case remaining of
                            value : rest -> next mem (value : stack) rest outputs branches
                            [] -> Left "Concrete replay requested an unavailable input."
                        (Print, value : rest) -> next mem rest remaining (show value : outputs) branches
                        (Store, addr : value : rest) -> next (M.insert addr value mem) rest remaining outputs branches
                        (Load, addr : rest) -> case M.lookup addr mem of
                            Just value -> next mem (value : rest) remaining outputs branches
                            Nothing -> Left "Concrete replay read an uninitialized variable."
                        (JmpIf, cond : addr : rest)
                            | addr < 0 || addr >= toInteger (length instructions) -> Left "Invalid concrete jump destination."
                            | otherwise ->
                                let taken = cond /= 0
                                    target = if taken then fromInteger addr else pc + 1
                                 in walk (fuel - 1) target mem rest remaining outputs (BranchEvent pc taken : branches)
                        (Add, _) -> binary (+)
                        (Lt, _) -> binary (\l r -> boolean (l < r))
                        (Eq, _) -> binary (\l r -> boolean (l == r))
                        (And, _) -> binary (\l r -> boolean (l /= 0 && r /= 0))
                        (Or, _) -> binary (\l r -> boolean (l /= 0 || r /= 0))
                        (Not, value : rest) -> next mem (boolean (value == 0) : rest) remaining outputs branches
                        (Pop, _ : rest) -> next mem rest remaining outputs branches
                        (Dup, value : rest) -> next mem (value : value : rest) remaining outputs branches
                        (Swap, l : r : rest) -> next mem (r : l : rest) remaining outputs branches
                        (Over, value : rest) -> next mem (value : rest ++ [value]) remaining outputs branches
                        (RotL, value : rest) -> next mem (rest ++ [value]) remaining outputs branches
                        _ -> Left "Concrete replay encountered an invalid stack shape."
    boolean True = 1
    boolean False = 0
