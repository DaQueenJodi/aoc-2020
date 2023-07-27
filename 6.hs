import Data.List (elemIndex, nub)
import Data.Maybe (isJust, fromJust)

split :: Eq a => a -> [a] -> [[a]]
split e xs
  | isJust idx = y : split e ys
  | otherwise = [xs]
  where
    (y, _:ys) = splitAt (fromJust idx) xs
    idx = e `elemIndex` xs

isRepeat :: Eq a => a -> [[a]] -> Bool
isRepeat e = all (e `elem`)

getIdentical :: Eq a => [[a]] -> [a]
getIdentical (xs:xss) = repeats
  where
    repeats = filter (\ x -> all (\ ys -> x `elem` ys) xss ) xs



solve :: String -> Int
solve s = sum $ map (length . getIdentical) groups
  where
    groups = split "" $ lines s
