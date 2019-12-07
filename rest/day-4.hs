module Main where

import Data.Char (digitToInt)
import Data.List (nub)

main :: IO ()
main = print . solution . reps $ range

range :: [Int]
range = [231832 .. 767346]

reps :: [Int] -> [[Int]]
reps = (fmap . fmap) digitToInt . map show

-- Solution
type State = (Int, Bool)

solution = length . filter predicate

test = monotonic . head . reps . (:[])

predicate :: [Int] -> Bool
predicate = (&&) <$> monotonic <*> oneDouble'

monotonic, oneDouble :: [Int] -> Bool
monotonic = checkAll (<=)
oneDouble = checkAny (==)

oneDouble' :: [Int] -> Bool
oneDouble' xs = let doubles = filter (uncurry (==)) . pairs $ xs
                    oneMatch x = (==1) . length $ filter (==x) doubles
                in length doubles > 0 && any oneMatch doubles

checkOne :: (Int -> Int -> Bool) -> [Int] -> Bool
checkOne f = (==1) . length . filter (uncurry f) . pairs

checkAll :: (Int -> Int -> Bool) -> [Int] -> Bool
checkAll f = all (uncurry f) . pairs

checkAny :: (Int -> Int -> Bool) -> [Int] -> Bool
checkAny f = any (uncurry f) . pairs

pairs = zip <$> id <*> tail
