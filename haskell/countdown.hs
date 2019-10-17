import Data.List

data Op   = Add | Sub | Mul | Div
data Expr = Val Int | App Op Expr Expr

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App operator left right) = [apply operator x y | x <- eval left, y <- eval right, valid operator x y]

findLast :: [a] -> a
findLast [x] = x
findLast (_:xs) = findLast xs

findLastButOne :: [a] -> a
findLastButOne [x,_] = x
findLastButOne (_:xs) = findLastButOne xs

safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs  

myRemove :: Int -> [a] -> [a]
myRemove _ [] = []
myRemove 0 xs = xs
myRemove n (_:xs) = myRemove (n-1) xs

safehead :: [a] -> a
safehead (x:_) = x

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x] 

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) 
  | x == head xs =     compress xs -- if current element equals the next, simply go ahead
  | otherwise    = x : compress xs -- otherwise concatenate the current element and go ahead
  
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) 
  | x == head xs = (x : head (pack xs)) : tail (pack xs)
  | otherwise    = [x] : pack xs

encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(myLength x, safehead x) | x <- pack xs]

dropEvery [] _ = []
--dropEvery xs 1 = []
dropEvery (x:xs) n | myLength xs < n = xs
				   | otherwise = 
--choices' :: [a] -> [a]
--choices' [] = []
--choices' (x:xs) = (x:xs) : [(y:xs) | y:_ <- choices' xs]
--[z | z <- xs, z /= x] : [y | y <- xs, y /= x]

selections' [] = []
selections' (x:xs) = (x, x:xs) : [(x, ys) | (_,ys) <- selections' xs]

selections :: [a] -> [(a,[a])]
selections []     = []
selections (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- selections xs] 

permutations' [] = [[]]
permutations' xs = [y:zs | (y, ys) <- selections xs, zs <- permutations' ys]
