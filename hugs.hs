import Data.Char

twice f x = f (f x)
getFirst n 	| n >= 1 && n < 10 = n
			| n > 10           = getFirst (n `div` 10)
getLength n	| n < 10    = 1
			| otherwise = getLength (n `div` 10) + 1
getTail n 	| n < 10    = n
			| otherwise = n - ((getFirst n) * (10^(getLength n - 1)))
toList n 	| getLength n == 1 = n:[]
			| otherwise = (getFirst n):(toList (getTail n))
halve xs = (take n xs, drop n xs, length xs)
	where n = length xs `div` 2
remove n xs = take n xs ++ drop (n+1) xs

primes :: Int -> [Int]
isPrime n = ([x | x <- [1..n], n `mod` x == 0] == [1,n])
primes n = [x | x <- [1..n], isPrime x]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

lucas 0 = 2
lucas 1 = 1
lucas n = lucas (n-1) + lucas (n-2)

sum100 = sum [x | x <- [1..100]]

squares n = [x^2 | x <- [1..n]]
squares' m n = [x^2 | x <- [n+1..n+m]]
sumSquares' x = sum . uncurry squares' $ (x, x)
coords m n = [(x,y) | x <- [0..m], y <- [0..n]]

tuple1 ([a],[b]) = [a]
tuple2 ([a],[b]) = [x | x <- [b..b]]

pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors n = [x | x <- [1..n], n `mod` x == 0 && x /= n]

perfects n = [x | x <- [1..n], isPerfect x]
	where isPerfect num = sum (factors num) == num

let2int :: Char -> Int
let2int c 
  | isUpper c = ord c - ord 'A'
  | otherwise = ord c - ord 'a'

int2let :: Char -> Int -> Char
int2let c n = chr (ord c + n)

shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2let 'a' ((let2int c + n) `mod` 26)
  | isUpper c = int2let 'A' ((let2int c + n) `mod` 26)
  | otherwise = c --i.e. space
--  where x = if isUpper c then 'A' else 'a' 

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]

int2bit 0 = []
int2bit n = n `mod` 2 : int2bit (n `div` 2)

evens xs = [x | x <- xs, even x]

first (x,_) = x

fib' f n = [(x, f x) | x <- [1..n]]