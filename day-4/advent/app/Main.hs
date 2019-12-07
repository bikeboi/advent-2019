module Main where

import Lib

main :: IO ()
main = do mem <- readNums "src/input/input-5.txt"
          let machine = initialize mem
          runIC machine test >>= print

file = "./src/input/input-5.txt"

test = execute

readNums :: FilePath -> IO [Int]
readNums fp = readFile fp >>= pure . map read . words . commaSpace
  where commaSpace = map (\c -> if c == ',' then ' ' else c)
