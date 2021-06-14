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