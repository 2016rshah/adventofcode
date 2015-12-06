import Data.List

threeVowels :: String -> Bool
threeVowels s = length vs >= 3
  where vs = filter (\c -> elem c "aeiou") s

duplicate :: String -> Bool
duplicate (x:y:zs) = x==y || duplicate (y:zs) 
duplicate _ = False

passesBlacklist :: String -> Bool
passesBlacklist s = not (foldr (||) False doesContain)
  where
    bl = ["ab", "cd", "pq", "xy"]
    doesContain = map (\e -> isInfixOf e s) bl

isNice1 :: String -> Bool
isNice1 s = (threeVowels s) && (duplicate s) && (passesBlacklist s)

divorce :: String -> Bool
divorce (x:y:z:es) = x==z || divorce (y:z:es)
divorce _ = False

twins :: String -> Bool
twins (x:y:zs) = isInfixOf [x,y] zs || twins (y:zs)
twins _ = False
--Caught the fact that I need to call twins on (y:zs) instead of just zs thanks to
--https://github.com/tylerjl/adventofcode/blob/master/src/Y2015/D05.hs

isNice2 :: String -> Bool
isNice2 s = twins s && divorce s

main = do
  inp <- readFile "input.txt"
  let strings = lines inp
  print $ (length . filter ((==) True) . map isNice1) strings
  print $ (length . filter ((==) True) . map isNice2) strings 

