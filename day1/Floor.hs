import Data.List

data P = L | R
	deriving (Eq, Ord, Show)

conv :: Char -> P
conv '(' = L
conv ')' = R

followInstruction :: Int -> P -> Int
followInstruction curr L = curr + 1
followInstruction curr R = curr - 1

path :: Int -> [P] -> [Int]
path curr (p:ps) = nxt : (path nxt ps)
	where nxt = followInstruction curr p
path curr _ = []

--path2 :: [P] -> [Int]
--path2 xs = foldl (\curr acc -> ((followInstruction (last acc) curr) : acc)) [0] xs

main = do
	inp <- readFile "input.txt"
	let instructions = map conv inp 
	print (last (path 0 instructions))
	print (elemIndex (-1) (path 0 instructions))
