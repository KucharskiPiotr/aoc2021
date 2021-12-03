import System.IO
import Control.Monad

data Command = Forward | Up | Down

navigate :: (Int, Int) -> Command -> (Int, Int)
navigate (horizontal, depth) command = case command of
  Forward -> (horizontal + 1, depth)
  Down -> (horizontal, depth + 1)
  Up -> (horizontal, depth - 1)

adaptCommand :: String -> Maybe Command
adaptCommand command = case command of
    "forward" -> Just Forward
    "down" -> Just Down
    "up" -> Just Up
    _ -> Nothing  

main :: IO()
main = do
    handle <- openFile "day2.txt" ReadMode
    contents <- hGetContents handle
    let entries = lines contents
    
    hClose handle