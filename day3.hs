{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad
import Data.List.Split
import System.IO

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
  bitListToInt $ map (\b -> if predicate b then 1 else 0) bits

generateGamma :: Int -> [Int] -> Int
generateGamma threshold = generateParameter (> threshold)

generateEpsilon :: Int -> [Int] -> Int
generateEpsilon threshold = generateParameter (< threshold)

main :: IO ()
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
bitSum (b : bs) = case b of
  '0' -> bitSum bs
  '1' -> 1 + bitSum bs
  _ -> bitSum bs

getRating :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Int -> [String] -> String
getRating _ _ _ _ [result] = result
getRating whenMoreOnes whenMoreZeros whenEqual index numberStrings = do
  let doubleSum = (*) 2 $ bitSum $ map (!! index) numberStrings
  let numberCount = length numberStrings
  getRating whenMoreOnes whenMoreZeros whenEqual (index + 1) $
    filter
      ( \ns ->
          (doubleSum > numberCount && whenMoreOnes (ns !! index))
            || (doubleSum < numberCount && whenMoreZeros (ns !! index))
            || (doubleSum == numberCount && whenEqual (ns !! index))
      )
      numberStrings

getOxygenRating :: Int -> [String] -> String
getOxygenRating =
  getRating
    (== '1')
    (== '0')
    (== '1')

getCo2Rating :: Int -> [String] -> String
getCo2Rating = 
  getRating
    (== '0')
    (== '1')
    (== '0')

binToInt :: String -> Int
binToInt binary =
  bitListToInt (map (read . pure :: Char -> Int) binary)

mainPart2 :: IO ()
mainPart2 = do
  handle <- openFile "day3.txt" ReadMode
  contents <- hGetContents handle
  let numberLines = lines contents
  let res = binToInt $ getOxygenRating 0 numberLines
  let co2 = binToInt $ getCo2Rating 0 numberLines
  print res
  print co2
  print (res * co2)
  hClose handle