module Main where

myLength [] = 0
myLength [x] = 1
myLength (x:xs) = myLength xs + 1

myFoldr f z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFoldl f z []     = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

elementAt (x:_) 1 = x
elementAt (x:xs) pos = elementAt xs (pos - 1)

mySubstring [] _ _ = []
mySubstring (x:_) _ 0 = []
mySubstring xs start n = elementAt xs start : mySubstring xs (start+1) (n-1)
-- [1,2,3,4,5] 3 2
-- [3,4]

myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs


myReverse [] = []
myReverse [a,b] = [b,a]
myReverse (x:xs) = myReverse xs ++ [x]

myIsPalindrome xs = xs == myReverse xs
-- to do: refactor to use halved lists

myHead (x:xs) = x
myTail (x:xs) = xs

myDrop _ [] = []
myDrop 0 (x:xs) = x:xs
myDrop n (x:xs) = myDrop (n-1) xs
--myDrop n xs = mySubstring xs (n+1) (myLength xs - n)

-- 2 [1,2,3,4,5]
-- [3,4,5]

myCompress [] = []
myCompress [x] = [x]
myCompress (x:xs) = if x == head xs then myCompress xs else x : myCompress xs
--if shouldCompress x == True then x : myCompress xs else myCompress xs

--"aaaabccaadeeee"
--"abcade"

--myTakeWhile _ [] = []
--myTakeWhile cond (x:xs)  
--  | (cond x == True) && shouldStop == True = (x : myTakeWhile cond xs) 
--  | otherwise = myTakeWhile cond xs
--  where
-- 	shouldStop = 

--myTakeWhile _ [] = []
--myTakeWhile cond (x:xs) = myFoldr (:) 0 (myTakeWhile (cond x == True ) xs)

myDropWhile _ [] = []
myDropWhile cond (x:xs)
  | (cond x == True) = myDropWhile cond xs
  | otherwise = x : myDropWhile cond xs

isElementOf a [] = False
isElementOf a (x:xs) = if a == x then True else isElementOf a xs

myPack [] = []
myPack [x] = [[x]]
--myPack (x:xs) = (x : myTakeWhile (== x) xs) : myPack (myDropWhile (== x) xs)
myPack (x:xs) = if x == (head . head $ myPack xs) 
	            then (x : (head $ myPack xs)) : (tail $ myPack xs)
	            else [x] : myPack xs