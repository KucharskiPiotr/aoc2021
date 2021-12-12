{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad
import Data.List.Split
import System.IO

stringNumberListToIntList :: String -> String -> [Integer]
stringNumberListToIntList delimiter s = 
  map (\x -> read x :: Integer) $ splitOn delimiter s

parseBoard :: [String] -> [Integer]
parseBoard = map stringNumberListToIntList " "

main :: IO ()
main = do
  handle <- openFile "day4.txt" ReadMode
  contents <- hGetContents handle
  let inputText = lines contents
  let inputDraw = head inputText
  let boardTexts = drop 2 inputText

  let drawnNumbers = stringNumberListToIntList "," $ map head inputText

  hClose handle