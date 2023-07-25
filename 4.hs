import Debug.Trace (trace)
import Data.List (findIndex)
import Data.Maybe ( fromJust, isJust)
import Data.Char (isSpace, isNumber)

import Text.Read (readMaybe)


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
type Passport = [(String, String)]

getVal :: String -> [(String, a)] -> Maybe a
getVal _ [] = Nothing
getVal s (x:xs) = if k == s then Just v else getVal s xs
  where
    (k, v) = x



passportChecks :: [(String, String -> Bool)]
passportChecks =
  [ ("byr", \x -> let xInt = readMaybe x in
     length x == 4 && isJust xInt && fromJust xInt >= 1920 && fromJust xInt <= 2002)
  , ("iyr", \x -> let xInt = readMaybe x in
     length x == 4 && isJust xInt && fromJust xInt >= 2010 && fromJust xInt <= 2020)
  , ("eyr", \x -> let xInt = readMaybe x in
     length x == 4 && isJust xInt && fromJust xInt >= 2020 && fromJust xInt <= 2030)
  , ("hgt", \x -> let xInt = readMaybe (takeWhile isNumber x)
                      unit = dropWhile isNumber x
                  in isJust xInt && case unit of
                      "cm" -> fromJust xInt >= 150 && fromJust xInt <= 193
                      "in" -> fromJust xInt >= 59 && fromJust xInt <= 76
                      _    -> False)
  , ("hcl", \x -> length x == 7 && all (\ x -> isNumber x || (x <= 'f' && x >= 'a')) (tail x))
  , ("ecl", \x -> x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
  , ("pid", \x -> length x == 9 && all isNumber x)
  ]

solve :: String -> Int
solve s = length $ filter valid (passports s)
  where
    valid :: Passport -> Bool
    valid passport = trace (show passport ++ "\n" ++ show res) res
      where
        res = all (\ (k, f) -> let maybeV = getVal k passport
                                     in isJust maybeV && f (fromJust maybeV)) passportChecks

    passports :: String -> [Passport]
    passports s = map mkPassport psRaw
      where
        psRaw = map (split isSpace) chunks
        chunks = map unwords $ split null ls
        ls = lines s
    mkPassport :: [String] -> Passport
    mkPassport = map (\ x -> let [key, val] = split (== ':') x in (key, val))
