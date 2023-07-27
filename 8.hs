
import Debug.Trace (traceShowId, traceShow)
import Data.Maybe (isNothing)
import Data.List (findIndices)


type Argument = Int
data Operator
  = Jmp
  | Nop
  | Acc
  deriving (Eq, Show)

data Instruction = Instruction Operator Argument
  deriving (Eq, Show)

parseInstruction :: String -> Instruction
parseInstruction s = Instruction op arg
  where
    arg = read (dropWhile (=='+') argRaw)
    op = case opRaw of
      "jmp" -> Jmp
      "nop" -> Nop
      "acc" -> Acc
    [opRaw, argRaw] = words s

data State = State
  { instructionPointer :: Int
  , instructions :: [Instruction]
  , accumulator :: Int
  } deriving Show

newState :: [Instruction] -> State
newState ins = State
  { instructionPointer = 0
  , instructions = ins
  , accumulator = 0
  }


next :: State -> State
next s@(State { instructionPointer = ip, instructions = ins, accumulator = acc })
  = case ins !! ip of
      (Instruction Jmp arg) -> s { instructionPointer = ip + arg }
      (Instruction Nop _) -> s { instructionPointer = ip + 1 }
      (Instruction Acc arg) -> s { instructionPointer = ip + 1, accumulator = acc + arg }

data Finished
  = Loop Int
  | Term Int
  deriving Show

run :: State -> Finished
run s = go s []
  where
    go s acc
      | instructionPointer s >= length (instructions s) = Term $ accumulator s
      | newIp `elem` acc = Loop $ accumulator s
      | otherwise = go ns (newIp : acc)
      where
        newIp = instructionPointer ns
        ns = next s


isOp :: Operator -> Instruction -> Bool
isOp op (Instruction operator _) = operator == op

replaceInstruction :: Int -> Operator -> [Instruction] -> [Instruction]
replaceInstruction n operator inss =  take n inss ++ [newIns] ++ drop (n + 1) inss
  where
    newIns = case inss !! n of
      (Instruction _ x) -> Instruction operator x

fixOp :: Operator -> Operator -> [Instruction] -> [Instruction]
fixOp op newOp ins = if null res then [] else head res
  where
    res = filter (isTerm . run . newState) replaced
    replaced = map (\ x -> replaceInstruction x newOp ins) idxs
    idxs = findIndices (isOp op) ins
    isTerm x = case x of 
      (Term _) -> True
      (Loop _) -> False

solve :: String -> Int
solve s = case res of
  (Term x) -> x
  (Loop _) -> error "unreachable"
  where
    ins = map parseInstruction $ lines s
    res = case (traceShowId $ fixOp Jmp Nop ins, fixOp Nop Jmp ins) of
      ([], x) -> run $ newState x
      (x, []) -> run $ newState x
