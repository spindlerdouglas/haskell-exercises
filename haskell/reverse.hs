removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = x:(removeLast xs)


myReverse []  = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

main = putStrLn "Hello World"


errorMsg = "List is empty"

getLast [] = error errorMsg
getLast [x] = x
getLast (x:xs) = getLast xs


getDivisors :: Int -> [Int]
getDivisors 0 = []
getDivisors 1 = []
getDivisors n = [x | x <- [2..n-1], mod n x == 0]


isPrime 1 = False
isPrime 2 = True
isPrime n = length (getDivisors n) == 0

isEven :: Int -> Bool
isEven n | (mod n 2 == 0) = True
		 | otherwise      = False

isOdd n = not $ isEven n