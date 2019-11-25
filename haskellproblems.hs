--module Main where

-- 1) find the last element of a list
mylast [] = Nothing
mylast [x] = Just x
mylast (x:xs) = mylast xs

-- 2) find the last but one element of a list
mylastbutone [] = Nothing
mylastbutone [x] = Nothing
mylastbutone [x,y] = Just x
mylastbutone (_:xs) = mylastbutone xs

-- 3) Find the K'th element of a list. The first element in the list is number 1.
elementAt [] _ = Nothing
elementAt (x:_) 1 = Just x
elementAt (_:xs) pos = elementAt xs (pos - 1)

-- 4) Find the number of elements of a list.
mylength [] = 0
mylength (x:xs) = mylength xs + 1
-- with accumulator
mylength' [] = 0
mylength' xs = length' xs 0
  where 
    length' [] acc = acc
    length' (x:xs) acc = length' xs (acc + 1) 
-- with fold
len_with_fold [] = 0
len_with_fold xs = foldr count' 0 xs
  where count' _ x = 1 + x

-- 5) Reverse a list.
myreverse [] = []
myreverse [a,b] = [b,a]
myreverse (x:xs) = myreverse xs ++ [x]
-- with fold
myreverse' [] = []
myreverse' xs = foldl (\x y -> y:x) [] xs 

-- 6) Find out whether a list is a palindrome.
-- implementation of auxiliary functions first
iseven [x] = False
iseven xs = iseven' xs False
  where
    iseven' [x] even = even
    iseven' (x:xs) even = iseven' xs (not even)

mytake _ [] = []
mytake 1 (x:xs) = [x]
mytake n (x:xs) = x : mytake (n-1) xs

myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (x:xs) = myDrop (n-1) xs
--myDrop n xs = mySubstring xs (n+1) (myLength xs - n)
-- 2 [1,2,3,4,5]
-- [3,4,5]

mysplitat [] _ = []
mysplitat xs pos = 
  let first = mytake pos xs
      rest = myDrop pos xs
  in [first, rest]

-- naive
myispalindrome xs = xs == myreverse xs
-- using halved lists
myispalindrome' xs = 
  let pivot = if iseven xs then 0 else 1 
      splitlist = mysplitat xs ((mylength' xs - pivot) / 2)
      first = splitlist !! 0
      rest = myreverse $ splitlist !! 1
  in if first == rest then True else False


-- 7) (**) Flatten a nested list structure.
-- TODO

-- 8) (**) Eliminate consecutive duplicates of list elements.
myCompress [] = []
myCompress [x] = [x]
myCompress (x:xs) = if x == head xs then myCompress xs else x : myCompress xs
--if shouldCompress x == True then x : myCompress xs else myCompress xs
--"aaaabccaadeeee"
--"abcade"

-- 9) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
myPack [] = []
myPack [x] = [[x]]
--myPack (x:xs) = (x : myTakeWhile (== x) xs) : myPack (myDropWhile (== x) xs)
myPack (x:xs) | x == (head . head $ myPack xs) = (x : (head $ myPack xs)) : (tail $ myPack xs)
              | otherwise = [x] : myPack xs

-- 10) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
myencode xs = foldr (\x acc -> (mylength' x, head x) : acc) [] packedlist
  where packedlist = myPack xs
  -- let 
  --   list = head $ myPack (x:xs)
  --   reps = mylength' list
  --   elem = head list --first element in packed list (list within list)
  -- in [(reps, elem)]

mySubstring [] _ _ = []
mySubstring (x:_) _ 0 = []
mySubstring xs start n = elementAt xs start : mySubstring xs (start+1) (n-1)
-- [1,2,3,4,5] 3 2
-- [3,4]
myFoldr f z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFoldl f z []     = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter _ [] = []
myFilter cond (x:xs) | cond x = x : myFilter cond xs 
                     | otherwise = myFilter cond xs

myHead (x:_) = x
myTail (_:xs) = xs

myTakeWhile _ [] = []
myTakeWhile cond xs = foldr (\x y -> if cond x then x:y else y) [] xs 
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

