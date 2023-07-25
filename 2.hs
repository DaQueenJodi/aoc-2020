import Data.Char (isNumber)

main :: IO ()
main = do
  file <- readFile "2.txt"
  print $ solve2 file

solve :: String -> Int
solve s = go (lines s) 0
  where
    go [] n = n
    go (x:xs) n = go xs newN
      where
        newN = if count <= most && count >= least then n + 1 else n
        least, most :: Int
        least = read $ takeWhile (/= '-') x
        most = read $ takeWhile isNumber $  tail $ dropWhile (/= '-') x
        count = length $ filter (== letter) $ dropWhile (/= ':') x
        letter = head $ tail $ dropWhile (/= ' ') x

solve2 :: String -> Int
solve2 s = go (lines s) 0
  where
    go [] n = n
    go (x:xs) n = go xs newN
      where
        newN = let
                  [b1, b2] = map (== letter) [letters !! p1, letters !! p2]
                  letters = drop 4 $ dropWhile (/= ' ') x
               in if b1 `xor` b2 then n + 1 else n
        a `xor` b
          | a && b = False
          | a || b = True
          | otherwise = False
        p1, p2:: Int
        p1 = read (takeWhile (/= '-') x) - 1
        p2 = read (takeWhile isNumber $  tail $ dropWhile (/= '-') x) - 1
        letter = head $ tail $ dropWhile (/= ' ') x

