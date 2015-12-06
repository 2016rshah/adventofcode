import Data.Set hiding (map)

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

data House = H Int Int --House x y
           deriving (Show, Eq, Ord)
data Direction = U | D | L | R
               deriving (Show, Eq)                    

nextHouse :: Int -> Int -> Direction -> House
nextHouse x y d = H (nextX x d) (nextY y d)

nextX :: Int -> Direction -> Int
nextX x L = x-1
nextX x R = x+1
nextX x _ = x

nextY :: Int -> Direction -> Int
nextY y U = y+1
nextY y D = y-1
nextY y _ = y

parseD :: Char -> Direction
parseD '^' = U
parseD '<' = L
parseD '>' = R
parseD 'v' = D


houses :: Int -> Int -> [Direction] -> [House]
houses x y (d:ds) = nextHouse x y d : houses (nextX x d) (nextY y d) ds
houses x y _ = []

glue :: String -> [House]
glue s = houses 0 0 (map parseD s)


dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)

main = do
  inp <- readFile "input.txt"
  let directions = map parseD inp
  print $ length (mkUniq ((H 0 0) : (houses 0 0 directions)))
  let d1 = dropEvery 2 directions
  let d2 = dropEvery 2 (tail directions)
  print $ length (mkUniq ((H 0 0) : ((houses 0 0 d1) ++ (houses 0 0 d2))))
