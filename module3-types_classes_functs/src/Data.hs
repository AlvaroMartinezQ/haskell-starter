module Data where

double :: Int -> Int
double x = 2 * x

add :: Int -> Int -> Int
add x y = x + y

-- Calling a function from another function
multipleOf :: Integer -> Integer -> Bool
multipleOf x y = y `mod` x == 0

isEven :: Integer -> Bool
isEven z = multipleOf z 2

-- Tuples
addTuples :: (Int, Int) -> (Int, Int)
addTuples (x, y) = (x + y, y + x)

-- if | else
pair :: Int -> Bool
pair x = if x `mod` 2 == 0 then True else False

-- case
pair' :: Int -> Bool
pair' x = case (x `mod` 2) of
							0 -> True
							1 -> False
							
bigger :: (Int, Int) -> Int
bigger (x, y)
			| x > y = x
			| otherwise = y
			
-- Lists
-- Concat a list
con :: [Int] -> [Int] -> [Int]
con l1 l2 = l1 ++ l2

-- Reverse a list
rev :: [Int] -> [Int]
rev l1 = reverse l1

-- List length
len  :: [Int] -> Int
len l1 = length l1

-- First element from list
fir :: [Int] -> Int
fir l1 = head l1

-- Get the list except the first element
remList :: [Int] -> [Int]
remList l1 = tail l1

-- Get the last element
las :: [Int] -> Int
las l1 = last l1

-- Get the list except the last element
remList' :: [Int] -> [Int]
remList' l1 = init l1

-- Get n element from a list
getN :: [Int] -> Int -> Int
getN l1 pos = l1 !! pos -- If pos > l1.length the exception is not handled

-- Zip
zipIt :: [Int] -> [Char] -> [(Int, Char)]
zipIt l1 l2 = zip l1 l2 
-- Unzip is also present for data lists

-- List comprehension
comp :: Int -> Int -> [Int]
comp lower upper = [lower | lower <- [lower..upper]]

-- Patterns
isZero :: Int -> Bool
isZero 0 = True
isZer _ = False