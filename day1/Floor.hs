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

--Trying to reimplement path with a fold, but I don't know how to keep track of both the current floor number and the list of all floor numbers at the same time
--path2 :: [P] -> [Int]
--path2 xs = foldl (\curr acc -> ((followInstruction (last acc) curr) : acc)) [0] xs

main = do
	inp <- readFile "input.txt"
	let instructions = map conv inp 
	print (last (path 0 instructions))
	print (elemIndex (-1) (path 0 instructions))
