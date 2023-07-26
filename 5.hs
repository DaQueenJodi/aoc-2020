import Prelude hiding (traverse)
import Data.List (sort)
import Debug.Trace (trace)

traverse :: [Char] -> [Int] -> Int
traverse _ [x] = x
traverse (x:xs) tree = traverse xs newTree
  where
    newTree = f' tree
    f' = f $ length tree `div` 2
    f = case x of
      'F' -> take
      'B' -> drop
      'L' -> take
      'R' -> drop
      _   -> error "unreachable"

solve :: String -> Int
solve s = head [x | x <- [0..], x `notElem` seatIds, all (`elem` seatIds) [x + 1, x - 1] ]
  where
    seatIds = map (\ (r, c) -> (r * 8) + c) seats
    seats = map getSeat seatsRaw
    seatsRaw = lines s

getSeat :: String -> (Int, Int)
getSeat s = (row, col)
  where
    row = traverse (take 7 s) [0..127]
    col = traverse (drop 7 s) [0..7]


main :: IO ()
main = do
  file <- readFile "5.txt"
  print $ solve file
