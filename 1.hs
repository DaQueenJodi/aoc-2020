main :: IO ()
main = do
  file <- readFile "1.txt"
  print $ solve file


solve :: String -> Integer
solve s = head [ x * y * z | x <- ns, y <- ns, z <- ns, x + y + z == 2020 ]
  where
    ns :: [Integer]
    ns = map read (lines s)

