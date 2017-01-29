import Data.List

data Light = On | Off
           deriving (Show, Eq)

data Command = Toggle Int Int | TurnOff Int Int | TurnOn Int Int
             deriving (Show, Eq)

toCommand :: String -> Command
toCommand s
  | "turn off" `isInfixOf` s = TurnOff (read (ws !! 2)) (read (ws !! 4))
  | "turn on" `isInfixOf` s = TurnOn (read (ws !! 2)) (read (ws !! 4))
  | "toggle" `isInfixOf` s = Toggle (read (ws !! 1)) (read (ws !! 3))
  where
    ws = words s

toggle :: Light -> Light
toggle On = Off
toggle Off = On

turnOff :: Light -> Light
turnOff _ = Off

turnOn :: Light -> Light
turnOn _ = On

main = do
  inp <- readFile "input.txt"
  let commands = (map toCommand . map (delete ',') . map (delete ',') . lines) inp
  print $ head commands

--The comma in the command refers to coordinates so need to fix parser and data structure
--Maybe treat the tree as an array of values and create new data structure to hold coordinates and whether or not its on or off
-- `let xs = [(x,y) | x<-[0..9], y<-[0..9]]`

