{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.List (nub)
import Data.Char (isNumber)
import Data.Maybe (isJust, fromJust)
import Prelude hiding (getContents)


type Bag = String
type Contents = [(Int, Bag)]

data Line = Line
  { flavor :: Bag,
    contains :: Contents
  } deriving Show


chunks :: Int -> [a] -> [[a]]
chunks n xs = take n xs : chunks n (drop n xs)

-- takes a `bag` and returns every `bag` that can contain it
trackBag :: Bag -> [Line] -> [Bag]
trackBag _ [] = []
trackBag b (x:xs) = if any (\ (_, b') -> b == b') x.contains then x.flavor : trackBag b xs else trackBag b xs


trackBagFully :: Bag -> [Line] -> [Bag]
trackBagFully b xs = go [b] xs
  where
    go [] _ = []
    go (b:bs) xs = tracked ++ go (bs ++ tracked) xs
      where
        tracked = trackBag b xs


getContents :: Bag -> [Line] -> Contents
getContents b xs = contains $ head $ filter (\ Line { flavor = f } -> f == b) xs

getContained :: Bag -> [Line] -> [Bag]
getContained b xs = concatMap (uncurry replicate ) contents
  where
    contents = getContents b xs
getContainedFully :: Bag -> [Line] -> [Bag]
getContainedFully b xs = contents ++ concatMap (`getContainedFully` xs) contents
  where
    contents = getContained b xs

parseLine :: String -> Line
parseLine s = Line bag contents
  where
    bag = unwords . take 2 $ words s
    contents = zip nums bags
    bags = map (unwords . init) $ chunks 3 $ words bagsRaw
    (_:bagsRaw) = filter (not . isNumber) $ filter (`notElem` ".,") contentsRaw
    nums = map read $ filter (all isNumber) (words contentsRaw)
    contentsRaw = dropWhile (not . isNumber) s

solve :: String -> Int
solve s = length . nub $ trackBagFully "shiny gold" pls
  where
    pls = map parseLine $ lines s
