import Data.List.Split
import Data.List

-- newtype Box = [Int]

dimensionStrings :: String -> [String]
dimensionStrings = splitOn "x"

dimensions :: [String] -> [Int]
dimensions = map read

area :: [Int] -> Int
area [a,b,c] = 2 * (a1 + a2 + a3) + am
  where
    a1 = a * b
    a2 = b * c
    a3 = a * c
    am = minimum [a1,a2,a3]

ribbon :: [Int] -> Int
ribbon [a,b,c] = (2 * (a + b)) + a*b*c
--assume the array passed in is sorted

main = do
  inp <- readFile "input.txt"
  let boxes = (map dimensions . map dimensionStrings . lines) inp
  -- print (boxes)
  let totalArea = (sum . map area) boxes
  print totalArea
  let totalRibbon = (sum . map ribbon . map sort) boxes
  print totalRibbon
  --20x3x15
