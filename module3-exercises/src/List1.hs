module List1 where

import Data.Char

-- Given 3 numbers the function returns True if they're in order
inOrder :: Int -> Int -> Int -> Bool
inOrder x y z = if (x < y) && (y < z) then True else False

-- Given a 3 element tuple return it ordered
orderTuple :: (Int, Int, Int) -> (Int, Int, Int)
orderTuple (x, y, z)
	| (x > y) && (y > z) = (z, y, x)
	| (y > x) && (x > z) = (z, x, y)
	| (z > y) && (y > x) = (x, y, z)
	
-- Given a radious of a circumference return a 2-tuple with its length and area
cir :: Float -> (Float, Float)
cir val = (n1, n2) where
	n1 = (3.14 * 2 * val)
	n2 = (3.14 * (val * val))
	
-- Given a number return a list with its divisors
divis :: Int -> [Int]
divis number = [x | x <- [1..number], number `rem` x == 0]

-- Functions returns True if passed value is a digit
isDig :: Char -> Bool
isDig n
		| isDigit n = True
		| otherwise = False
		
-- Function determines if a number is prime
isPrime :: Int -> Bool
isPrime n = length(divis n) == 2

-- Given a list return another list with prime and odd from the received list
primesNodds :: [Int] -> [Int]
primesNodds l1 = [x | x <- l1, isPrime x, odd x]

-- Given a number return a list with its prime numbers
primesNumber :: Int -> [Int]
primesNumber num = [x | x <- [1..num], isPrime x]

-- 2-tuple lists are received
-- example: (c1, c2) if c2 isVocal then c1 is part of the hidden password in the tuples
-- test: hiddenPassword [('c','f'), ('a', 'e'), ('d', 'a'), ('e','c'), ('n','i'), ('a','j')]
hiddenPassword :: [(Char, Char)] -> String
hiddenPassword l1 = [c1 | (c1, c2) <- l1, isVocal c2]

isVocal :: Char -> Bool
isVocal val = (val == 'a') || (val == 'e') || (val == 'i') || (val == 'o') || (val == 'u')

-- Given a number and a 2-tuple list return those tuples if the first element is odd and >number
filterTuples :: Int -> [(Int, Int)] -> [(Int, Int)]
filterTuples num l1 = [(c1, c2) | (c1, c2) <- l1, odd c1, c1 > num]

-- Given a 3-tuple list return the number of those tuples which follow (c1,c2,c3): c1^2 + c2^2 = c3^2
pit :: [(Int, Int, Int)] -> Int
pit l1 = length [(c1,c2,c3) | (c1,c2,c3) <- l1, pitAux (c1,c2,c3)]

pitAux :: (Int, Int, Int) -> Bool
pitAux (c1,c2,c3) = c1*c1 + c2*c2 == c3*c3

-- Given a string return a list with each char ASCII value
asciiVal :: String -> [Int]
asciiVal str = [ord val | val <- str] -- ord comes from Data.Char import

-- Given a list of integers return a String with its first element and length
fisrtLength :: [Int] -> String
fisrtLength [] = "Empty list"
fisrtLength l1 = "List first element: " ++ show (head l1) ++ ". Length: " ++ show (length l1) -- Don't forget parenthesis!

-- Given a String return an Integer which counts all upper Char
allUpper :: String -> Int
allUpper str = length [val | val <- str, isUpper val]