module Main where
  import System.IO
  import Control.Monad
  import Data.List.Split

  data Command = Forward | Up | Down

  navigate :: (Int, Int) -> (Maybe Command, Int) -> (Int, Int)
  navigate (horizontal, depth) (command, amount) = case command of
    Nothing -> (horizontal, depth)
    Just c -> case c of
      Forward -> (horizontal + amount, depth)
      Down -> (horizontal, depth + amount)
      Up -> (horizontal, depth - amount)

  navigateAim :: (Int, Int, Int) -> (Maybe Command, Int) -> (Int, Int, Int)
  navigateAim (horizontal, depth, aim) (command, amount) = case command of
    Nothing -> (horizontal, depth, aim)
    Just c -> case c of
      Forward -> (horizontal + amount, depth + (aim * amount), aim)
      Down -> (horizontal, depth, aim + amount)
      Up -> (horizontal, depth, aim - amount)

  adaptCommand :: String -> Maybe Command
  adaptCommand command = case command of
      "forward" -> Just Forward
      "down" -> Just Down
      "up" -> Just Up
      _ -> Nothing  

  parseCommandLine :: String -> (Maybe Command, Int)
  parseCommandLine txt = (adaptCommand(head(splitOn " " txt)), read (last(splitOn " " txt)) :: Int)

  main :: IO()
  main = do
      handle <- openFile "day2.txt" ReadMode
      contents <- hGetContents handle
      let commands = map parseCommandLine $ lines contents
      let res = foldl navigate (0, 0) commands
      print res
      hClose handle

  mainPart2 :: IO()
  mainPart2 = do
      handle <- openFile "day2.txt" ReadMode
      contents <- hGetContents handle
      let commands = map parseCommandLine $ lines contents
      let res = foldl navigateAim (0, 0, 0) commands
      print res
      hClose handle