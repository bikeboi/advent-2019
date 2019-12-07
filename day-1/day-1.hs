module Main where

main :: IO ()
main = getInput >>= print . sum . map fuelFor

getInput :: IO [Integer]
getInput = readFile "input.txt" >>= pure . (map toInt) . words
  where toInt :: String -> Integer
        toInt = read

fuelFor :: Integer -> Integer
fuelFor x = let fuel = calcFuel x
            in if fuel <= 0 then 0 else fuel + fuelFor fuel
  where calcFuel n = ( n `div` 3 ) - 2
