{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO
import Control.Monad
import Data.List.Split

addBitValues :: [Int] -> String -> [Int]
addBitValues [] "" = []
addBitValues (latestSum : remainingSum) (latestBit : remainingBits) =
  case latestBit of
    '0' -> latestSum : addBitValues remainingSum remainingBits
    '1' -> (latestSum + 1) : addBitValues remainingSum remainingBits
    _ -> latestSum : remainingSum

bitListToInt :: [Int] -> Int 
bitListToInt [] = 0
bitListToInt bits = last bits + 2 * bitListToInt (init bits)

generateParameter :: (Int -> Bool) -> [Int] -> Int
generateParameter predicate bits = 
  bitListToInt $ map (\ b -> if predicate b then 1 else 0) bits

generateGamma :: Int -> [Int] -> Int
generateGamma threshold = generateParameter (>threshold)

generateEpsilon :: Int -> [Int] -> Int
generateEpsilon threshold = generateParameter (<threshold)

main :: IO()
main = do
  handle <- openFile "day3.txt" ReadMode 
  contents <- hGetContents handle
  let numberLines = lines contents
  let amountOfLines = length numberLines
  let threshold = amountOfLines `div` 2
  let numberLength = length $ head numberLines
  let bitSums = foldl addBitValues (replicate numberLength 0) numberLines
  let gamma = generateGamma threshold bitSums
  let epsilon = generateEpsilon threshold bitSums
  print gamma
  print epsilon
  print (gamma * epsilon)
  hClose handle

bitSum :: String -> Int
bitSum "" = 0
bitSum (b:bs) = case b of
  '0' -> bitSum bs
  '1' -> 1 + bitSum bs
  _ -> bitSum bs

getOxygenRating :: Int -> [String] -> String
getOxygenRating _ [result] = result
getOxygenRating index numberStrings = do
  let sum = bitSum $ map (!!index) numberStrings
  getOxygenRating (index + 1) $ 
    filter (\ns -> 
      (sum >= (length numberStrings `div` 2) && ns !! index == '1') || 
      (sum < (length numberStrings `div` 2) && ns !! index == '0')) numberStrings


getCo2Rating :: Int -> [String] -> String
getCo2Rating _ [result] = result
getCo2Rating index numberStrings = do
  let sum = bitSum $ map (!!index) numberStrings
  getOxygenRating (index + 1) $ 
    filter (\ns -> 
      (sum >= (length numberStrings `div` 2) && ns !! index == '0') || 
      (sum < (length numberStrings `div` 2) && ns !! index == '1')) numberStrings

mainPart2 :: IO()
mainPart2 = do
  handle <- openFile "day3.txt" ReadMode 
  contents <- hGetContents handle
  let numberLines = lines contents
  let res = getOxygenRating 0 numberLines
  let co2 = getCo2Rating 0 numberLines
  print res
  print co2
  hClose handle