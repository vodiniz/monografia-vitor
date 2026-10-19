module Symbolic where

import qualified Data.Map.Strict as M
import qualified Data.Tree as T
import Foundation
import Foundation.Collection
import Types
import Util

-- Keep the depth-only entry point for existing callers. Compiled source programs
-- should use symRunWith so the guard and exit of each canonical loop are known.
symRun :: Int -> Prog -> SymState -> Trace
symRun depth = symRunWith (defaultAnalysisConfig{maxDepth = depth}) emptyProgramInfo

symRunWith :: AnalysisConfig -> ProgramInfo -> Prog -> SymState -> Trace
symRunWith config info prog initial
    | loopK config < 0 = error "loop-k must be non-negative."
    | maxDepth config < 0 = error "max-depth must be non-negative."
    | otherwise = walk (maxDepth config) initial
  where
    walk depth original =
        let state@(pc, _, mem, stack, _, _) = enterLoop info original
            leaf reason = T.Node (TraceNode state reason) []
         in case prog ! (Offset pc) of
                Just Done -> leaf Completed
                Nothing -> leaf (ExecutionError "Program counter is outside the program.")
                Just instr
                    | depth <= 0 -> leaf CutByDepth
                    | invalidAccess instr mem stack -> leaf (ExecutionError "Invalid or uninitialized memory access.")
                    | symbolicJump instr stack -> leaf (ExecutionError "Symbolic jump destinations are unsupported.")
                    | otherwise ->
                        T.Node
                            (TraceNode state Running)
                            (fmap (follow depth state) (symStep state instr))

    -- The loop guard branches to the real exit or to the body. At j == k the
    -- latter is a cut, not an artificial fall-through to the code after the loop.
    follow depth (pc, _, _, _, _, before) next@(nextPc, _, _, _, _, after) =
        case guardAt info pc of
            Just loop
                | nextPc == loopBody loop
                , M.findWithDefault 0 (loopHeader loop) (loopCounts before) >= loopK config ->
                    T.Node (TraceNode next (CutByLoop (loopHeader loop))) []
                | nextPc == loopExit loop ->
                    walk (depth - 1) (leaveLoop (loopHeader loop) next)
            _
                | nextPc < pc
                , M.notMember nextPc (programLoops info)
                , M.findWithDefault 0 nextPc (loopCounts after) > loopK config ->
                    -- Hand-written bytecode may lack a canonical condition. Bound its
                    -- returns as a fallback without claiming body-iteration coverage.
                    T.Node (TraceNode next (CutByLoop nextPc)) []
            _ -> walk (depth - 1) next

-- A new activation starts only when its old counter is absent. The back edge
-- keeps the existing counter; normal exit removes it, including for inner loops.
enterLoop :: ProgramInfo -> SymState -> SymState
enterLoop info state@(pc, i, mem, stack, cs, context)
    | M.member pc (programLoops info) =
        let counts = M.insertWith (\_ old -> old) pc 0 (loopCounts context)
         in (pc, i, mem, stack, cs, context{loopCounts = counts})
    | otherwise = state

leaveLoop :: Int -> SymState -> SymState
leaveLoop header (pc, i, mem, stack, cs, context) =
    (pc, i, mem, stack, cs, context{loopCounts = M.delete header (loopCounts context)})

guardAt :: ProgramInfo -> Int -> Maybe LoopInfo
guardAt info pc = findGuard (M.elems (programLoops info))
  where
    findGuard [] = Nothing
    findGuard (loop : rest)
        | loopGuard loop == pc = Just loop
        | otherwise = findGuard rest

invalidAccess :: Instr -> M.Map Word32 Sym -> [Sym] -> Bool
invalidAccess Load mem (SCon addr : _) = M.notMember addr mem
invalidAccess Load _ _ = True
invalidAccess Store _ (SCon _ : _ : _) = False
invalidAccess Store _ _ = True
invalidAccess _ _ _ = False

symbolicJump :: Instr -> [Sym] -> Bool
symbolicJump JmpIf (_ : SCon _ : _) = False
symbolicJump JmpIf _ = True
symbolicJump _ _ = False

symStep :: SymState -> Instr -> [SymState]
symStep (pc, i, mem, l : r : stack, cs, ctx) Add = pure (pc + 1, i, mem, simplify (SAdd l r) : stack, cs, ctx)
symStep _ Add = error "Add expects two arguments."
symStep (pc, i, mem, stack, cs, ctx) Read =
    let ctx' = ctx{readEvents = ReadEvent i pc : readEvents ctx}
     in pure (pc + 1, i + 1, mem, SAny i : stack, cs, ctx')
symStep (pc, i, mem, stack, cs, ctx) (Push w) = pure (pc + 1, i, mem, SCon w : stack, cs, ctx)
symStep (pc, i, mem, _ : stack, cs, ctx) Pop = pure (pc + 1, i, mem, stack, cs, ctx)
symStep _ Pop = error "Pop expects one argument."
symStep (pc, i, mem, w : stack, cs, ctx) Dup = pure (pc + 1, i, mem, w : w : stack, cs, ctx)
symStep _ Dup = error "Dup expects one argument."
symStep (pc, i, mem, _ : stack, cs, ctx) Print = pure (pc + 1, i, mem, stack, cs, ctx)
symStep _ Print = error "Print expects one argument."
symStep (pc, i, mem, x : y : stack, cs, ctx) Swap = pure (pc + 1, i, mem, y : x : stack, cs, ctx)
symStep _ Swap = error "Swap expects two arguments."
symStep (pc, i, mem, cond : SCon addr : stack, cs, ctx) JmpIf =
    let destination = wordToInt addr
        successor taken constraints =
            let target = if taken then destination else pc + 1
                -- Detect the back edge exactly at a taken jump to a lower PC.
                counts =
                    if taken && destination < pc
                        then M.insertWith (+) destination 1 (loopCounts ctx)
                        else loopCounts ctx
                ctx' = ctx{loopCounts = counts, branchEvents = BranchEvent pc taken : branchEvents ctx}
             in (target, i, mem, stack, constraints, ctx')
     in case constantValue cond of
            Just 0 -> [successor False cs]
            Just _ -> [successor True cs]
            Nothing -> [successor False (SNot cond : cs), successor True (cond : cs)]
symStep _ JmpIf = error "JmpIf expects a condition and a concrete destination."
symStep (pc, i, mem, w : stack, cs, ctx) Over = pure (pc + 1, i, mem, w : stack <> [w], cs, ctx)
symStep _ Over = error "Over expects one argument."
symStep (pc, i, mem, w : stack, cs, ctx) RotL = pure (pc + 1, i, mem, stack <> [w], cs, ctx)
symStep _ RotL = error "RotL expects one argument."
symStep (pc, i, mem, w : stack, cs, ctx) Not = pure (pc + 1, i, mem, simplify (SNot w) : stack, cs, ctx)
symStep _ Not = error "Not expects one argument."
symStep (pc, i, mem, l : r : stack, cs, ctx) And = pure (pc + 1, i, mem, simplify (SAnd l r) : stack, cs, ctx)
symStep _ And = error "And expects two arguments."
symStep (pc, i, mem, l : r : stack, cs, ctx) Or = pure (pc + 1, i, mem, simplify (SOr l r) : stack, cs, ctx)
symStep _ Or = error "Or expects two arguments."
symStep (pc, i, mem, l : r : stack, cs, ctx) Lt = pure (pc + 1, i, mem, simplify (SLt l r) : stack, cs, ctx)
symStep _ Lt = error "Lt expects two arguments."
symStep (pc, i, mem, l : r : stack, cs, ctx) Eq = pure (pc + 1, i, mem, simplify (SEq l r) : stack, cs, ctx)
symStep _ Eq = error "Eq expects two arguments."
symStep (pc, i, mem, SCon addr : w : stack, cs, ctx) Store = pure (pc + 1, i, M.insert addr w mem, stack, cs, ctx)
symStep _ Store = error "Store expects a concrete address and a value."
symStep (pc, i, mem, SCon addr : stack, cs, ctx) Load =
    case M.lookup addr mem of
        Just w -> pure (pc + 1, i, mem, w : stack, cs, ctx)
        Nothing -> error "Nothing to load at address."
symStep _ Load = error "Load expects a concrete address."
symStep _ Done = error "No step for Done."

-- Fold only expressions whose complete value is known. Word32 arithmetic is
-- intentionally retained; source/VM differences are checked during export.
simplify :: Sym -> Sym
simplify expr = case constantValue expr of
    Just value -> SCon value
    Nothing -> expr

constantValue :: Sym -> Maybe Word32
constantValue (SCon value) = Just value
constantValue (SAny _) = Nothing
constantValue (SAdd l r) = (+) <$> constantValue l <*> constantValue r
constantValue (SEq l r) = boolToWord <$> ((==) <$> constantValue l <*> constantValue r)
constantValue (SLt l r) = boolToWord <$> ((<) <$> constantValue l <*> constantValue r)
constantValue (SNot c) = boolToWord . not . wordToBool <$> constantValue c
constantValue (SAnd l r) = boolToWord <$> ((&&) <$> (wordToBool <$> constantValue l) <*> (wordToBool <$> constantValue r))
constantValue (SOr l r) = boolToWord <$> ((||) <$> (wordToBool <$> constantValue l) <*> (wordToBool <$> constantValue r))

defaultSymState :: SymState
defaultSymState = (0, 0, M.empty, [], [], SymContext M.empty [] [])

renderSym :: Sym -> String
renderSym (SAdd l r) = "(" <> renderSym l <> " + " <> renderSym r <> ")"
-- Display unsigned constants consistently with the actual solver domain.
renderSym (SCon w) = show w
renderSym (SAny i) = valName i
renderSym (SEq l r) = renderSym l <> " = " <> renderSym r
renderSym (SNot c) = "~(" <> renderSym c <> ")"
renderSym (SAnd l r) = renderSym l <> " and " <> renderSym r
renderSym (SOr l r) = renderSym l <> " or " <> renderSym r
renderSym (SLt l r) = renderSym l <> " < " <> renderSym r
