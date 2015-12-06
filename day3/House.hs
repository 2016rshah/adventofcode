import Data.Set hiding (map)

--http://stackoverflow.com/a/18627837
mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

--http://stackoverflow.com/a/5290128
dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)

--Data structures for situation. House is its coordinates, and direction is either up, down, left, or right
data House = H Int Int --House x y
           deriving (Show, Eq, Ord)
data Direction = U | D | L | R
               deriving (Show, Eq)                    

-- Gets the location of the next house given coordinates of current house and direction
nextHouse :: Int -> Int -> Direction -> House
nextHouse x y d = H (nextX x d) (nextY y d)

--Pattern matching for Directions
nextX :: Int -> Direction -> Int
nextX x L = x-1
nextX x R = x+1
nextX x _ = x

--More pattern matching for directions
nextY :: Int -> Direction -> Int
nextY y U = y+1
nextY y D = y-1
nextY y _ = y

--Pattern matching for parsing the directions
parseD :: Char -> Direction
parseD '^' = U
parseD '<' = L
parseD '>' = R
parseD 'v' = D

--Gets the path of houses that santa will visit
houses :: Int -> Int -> [Direction] -> [House]
houses x y (d:ds) = nextHouse x y d : houses (nextX x d) (nextY y d) ds
houses x y _ = []

main = do
  inp <- readFile "input.txt"
  let directions = map parseD inp
  --Get first answer by getting the path of santa and removing any duplicate houses
  print $ length (mkUniq ((H 0 0) : (houses 0 0 directions)))
  --Santa one follows every other direction starting at the first direction
  let d1 = dropEvery 2 directions
  --Santa two follows every other direction starting at the second direction
  let d2 = dropEvery 2 (tail directions)
  --Get second answer by combining the houses that both santas visited, and remove any duplicates
  print $ length (mkUniq ((H 0 0) : ((houses 0 0 d1) ++ (houses 0 0 d2))))
