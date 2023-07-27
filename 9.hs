import Data.Maybe (fromJust)
import Data.List (elemIndex)
lastN :: Int -> [a] -> [a]
lastN n = take n . tail . reverse

solve :: String -> Int
solve s = fromJust $ False `elemIndex` map (`f` ns) [1..length $ drop 5 ns]
  where
    f :: Int -> [Int] -> Bool
    f n ns = not $ null [x | x <- ns', y <- ns', x + y == n']
      where
        n' = head ns'
        ns' = lastN 5 (take (n + 1) ns)
    ns = map read $ lines s



