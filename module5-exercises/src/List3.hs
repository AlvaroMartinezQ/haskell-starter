module List3 where

-- Represent a new data type to store a date
-- Try it out -> shell> Dt 17 03 2021
data Date = Dt {
				year :: Int,
				month :: Int,
				day :: Int
				} deriving Show

-- Order the elements of a list
orderElems :: (Eq a, Ord a) => [a] -> [a]
orderElems [] = []
orderElems [a] = [a]
orderElems (x:xs) = (orderElems less) ++ [x] ++ (orderElems great)
	where
		less = filter (< x) xs
		great = filter (>= x) xs
		
-- Given a list and a number return another list with the result of dividing each elemet from the first list with the element received
divAll :: (Fractional a) => a -> [a] -> [a]
divAll _ [] = []
divAll num (x:xs) = [num / x] ++ divAll num xs

-- Data types for a university
data Student = Stu {
					name :: String,
					degree :: Degree
					} deriving Show
					
data Degree = Degree2 | Degree2_ADE | ADE_Degree | Computer_Engineering deriving Show

pepe :: Student
pepe = Stu "pepe" Degree2

irene :: Student
irene = Stu "irene" ADE_Degree

alvaro :: Student
alvaro = Stu "alvaro" Computer_Engineering

students :: [Student]
students = [pepe, irene, alvaro]

-- Using the data type Date from above
-- Given two dates return True if they're the same
-- Try it out -> shell> sameDate (Dt 1999 02 23) (Dt 1999 02 23)
sameDate :: Date -> Date -> Bool
sameDate (Dt y1 m1 d1) (Dt y2 m2 d2) = y1==y2 && m1==m2 && d1==d2


-- Create a new class to represent data collections
class Collection c where
	isEmpty :: c b -> Bool
	insert :: b -> c b -> c b
	first :: c b -> b
	delete :: c b -> c b
	size :: c b -> Int
	
data Stack a = Sta [a] deriving Show
data Queue a = Que [a] deriving Show

instance Collection Stack where
	isEmpty (Sta s) = length s == 0
	insert val (Sta s) = Sta (s ++ [val])
	first (Sta s) = head s
	delete (Sta s) = Sta (init s)
	size (Sta s) = length s
	
instance Collection Queue where
	isEmpty (Que a) = length a == 0
	insert val (Que a) = Que (a ++ [val])
	first (Que a) = last a
	delete (Que a) = Que (init a)
	size (Que a) = length a