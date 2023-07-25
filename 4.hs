import Debug.Trace (trace)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Char (isSpace)


main :: IO ()
main = do
  file <- readFile "4.txt"
  print $ solve file

split :: Eq a => (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split f xs
  | any f xs = p1 : split f p2
  | otherwise = [xs]
  where
    (p1, _:p2) = splitAt idx xs
    idx = fromJust (f `findIndex` xs)
type Passport = [String]

solve :: String -> Int
solve s = length $ filter valid (passports s)
  where
    valid passport = all ((== True) . (`elem` passport))
      [ "byr"
      , "iyr"
      , "eyr"
      , "hgt"
      , "hcl"
      , "ecl"
      , "pid"
      ]
    passports :: String -> [Passport]
    passports s = ps'
      where
        ps :: [Passport]
        ps' = map (map (takeWhile (/= ':'))) ps
        ps = trace (show (map (split isSpace) chunks)) (map (split isSpace) chunks)
        chunks = map unwords $ split null ls
        ls = lines s
