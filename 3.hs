main :: IO ()
main = do
  file <- readFile "3.txt"
  print $ solve file

solve :: String -> Int
solve s = product tries
  where
    tries = map (\ (x, y) -> go ls x y 0) [ (1, 1)
                                          , (3, 1)
                                          , (5, 1)
                                          , (7, 1)
                                          , (1, 2)
                                          ]
    ls = map (concat . repeat) (lines s)
    go :: [String] -> Int -> Int -> Int -> Int
    go [x] _ _ n = if head x == '#' then n + 1 else n
    go (x:ys) right down n = go (map (drop right) (drop (down - 1) ys)) right down newN
      where
        newN = if tree then n + 1 else n
        tree = head x == '#'
