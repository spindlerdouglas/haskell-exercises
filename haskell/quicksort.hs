module Quicksort where


quicksort [] = []
quicksort (x:xs) = quicksort leftX ++ [x] ++ quicksort rightX
					where
						leftX  = [y | y <- xs, y < x]
						rightX = [y | y <- xs, y >= x] 


myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome xs = xs == myReverse xs


getCharAt _ [] = error "empty list"
getCharAt 0 _ = error "invalid index"
getCharAt 1 (x:xs) = x
getCharAt n (_:xs) = getCharAt (n-1) xs


getElems _ [] = error "empty list"
getElems 0 _ = error "invalid index"
getElems _ [x] = [x]
getElems 1 (x:_) = [x] 
getElems n (x:xs) = x:getElems(n-1) xs

mySplitAt 1 xs = xs
mySplitAt n (x:xs) = mySplitAt (n-1) xs

getLast [] = error "empty list"
getLast	[x] = x
getLast (x:xs) = getLast xs

toList x = [x]


myRemoveAt _ [] = []
myRemoveAt 1 (_:xs) = xs
myRemoveAt n xs = getElems (n-1) xs ++ mySplitAt (n+1) xs

getLength [] = 0
getLength [x] = 1
getLength (x:xs) = 1 + getLength xs 

isEven n = (mod n 2 == 0)
isOdd n = not $ isEven n

isPalindrome' xs  -- | getLength xs == 1                = True
				 | (isEven (getLength xs) == True ) = getElems ((getLength xs) / 2)             xs == myReverse (mySplitAt ((getLength xs) / 2 + 1) xs)
				 | otherwise                        = getElems ((((getLength xs) + 1) / 2) - 1) xs == myReverse (mySplitAt ((((getLength xs) + 1) / 2) + 1) xs)


isPalindrome'' xs = take halfLen xs == reverse (drop (halfLen + (len `mod` 2)) xs)
	where
		len = getLength xs
		halfLen = len `div` 2

myFilter f xs = [x | x <- xs, f x == True]