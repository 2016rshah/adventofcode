import Data.Char

collapse :: [Int] -> [Int]
collapse arr@(x:xs) = [length arr, x]

partition :: [Int] -> [[Int]]
partition arr@(x:xs) = (takeWhile ((==) x) arr) : partition (dropWhile ((==) x) arr)
partition _ = []

readInp :: String -> [Int]
readInp = map digitToInt  

lookAndSay :: [Int] -> [Int]
lookAndSay = concat . map collapse . partition 

answer :: String -> Int -> Int
answer s n = length $ (iterate lookAndSay (readInp s)) !! n

main = do
  inp <- readFile "input.txt"
  print $ answer inp 40
  print $ answer inp 50
