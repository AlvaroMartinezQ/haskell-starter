module List2 where

import Data.Char

-- Given a String and a Char return a counter with all the repetitions of the Char in the String
countChar :: String -> Char -> Int
countChar [] _ = 0
countChar (x:xs) c = if x == c then 1 + countChar xs x else countChar xs c

-- Given a 3-tuple composed of 2-tuples (String, Int) return a 3-tuple with the 1st element of each internal tuple
firstElem :: ((String, Int), (String, Int), (String, Int)) -> (String, String, String)
firstElem ((s1, _), (s2, _), (s3, _)) = (s1, s2, s3)

-- Given a list of Integers, return True if the add of the first 4 elements is lower than 10, ioc return False
addSmallerTen :: [Int] -> Bool
addSmallerTen l1 = addSmallerTenAux l1 0 0

addSmallerTenAux :: [Int] -> Int -> Int -> Bool
addSmallerTenAux [] c _ = c < 10
addSmallerTenAux (x:xs) c i = if i == 4 then c < 10 else addSmallerTenAux xs (c+x) (i+1)

-- Given a number and a list return True if all elements of the list are equal to the number
allEqual :: [Int] -> Int -> Bool
allEqual [] _ = True
allEqual (x:xs) n = if x == n then allEqual xs n else False

-- Given a String return a String with the first letter and last letter of the original String
firstLast :: String -> String
firstLast str = "First letter from original String " ++ show (str) ++ " is: " ++ show (head (str)) ++ ". Last letter is: " ++ show (last (str))

-- Given an Integer return different Strings depending on its range n<10, n>10 && n<20, n>20
rangeNumber :: Int -> String
rangeNumber n
		| n <= 10 = lw
		| n > 10 && n <= 20 = bt
		| n > 20 = gt
		where
			lw = "Number is lower than 10"
			bt = "Number is between 10 and 20"
			gt = "Number is greater than 20"
			
-- Given 2 numbers they're friends if the addition of its divisors equal the other number
--		*List2> friends 220 284
--			True
--		*List2> friends 15 5
--			False
friends :: Int -> Int -> Bool
friends n1 n2 = addN1 == n2 && addN2 == n1 where
	addN1 = sum (ownDivs n1)
	addN2 = sum (ownDivs n2)

ownDivs :: Int -> [Int]
ownDivs l = [x | x <- [1..l-1], l `rem` x == 0]

-- Given a String return the count of consonants
countCons :: String -> Int
countCons str = length [x | x <- str, isVocal (toLower x) == False, isLetter x]

isVocal :: Char -> Bool
isVocal val = (val == 'a') || (val == 'e') || (val == 'i') || (val == 'o') || (val == 'u')


-- Given a number return the Mersenne list of the number
mersenne :: Int -> [Int]
mersenne n = take n [x | y <- [1..], isPrime y, let x = (2^y-1)] -- As the list doesn't stop we take the first n numbers

divis :: Int -> [Int]
divis number = [x | x <- [1..number], number `rem` x == 0]
		
isPrime :: Int -> Bool
isPrime n = length(divis n) == 2

-- Given 2 lists return True if they're equal (length and element by element)
sameList :: [Int] -> [Int] -> Bool
sameList l1 l2
		| length l1 /= length l2 = False
		| otherwise = length l1 == length l3 where
			l3 = [1 | (x, y) <- zip l1 l2, x == y]
			
-- Given a list return its head (first element)
first :: [Int] -> Int
first [] = 0
first (x:xs) = x

-- Given a list return its tail (all elements except the first)
listTail :: [Int] -> [Int]
listTail [] = []
listTail (x:xs) = xs

-- Given 2 numbers return its cocient or remainder (the bigger one)
biggerNum :: Int -> Int -> Int
biggerNum n1 n2 = if c > r then c else r
	where
		c = n1 `div` n2
		r = n1 `rem` n2
		
-- Given a number 0 to 10 return a String
passed :: Float -> String
passed n
		| n < 5.0 = s1
		| n >= 5.0 && n < 9.0 = s2
		| n >= 9.0 = s3
		where
			s1 = "Failed"
			s2 = "Passed"
			s3 = "Super passed"
			
-- Given a list return all evens^2
cuad :: [Int] -> [Int]
cuad l1 = [x*x | x <- l1, even x]

-- Given a list return a 2-tuple list with (element, element position in original list)
posList :: [Int] -> [(Int, Int)]
posList l1 = posListAux l1 0

posListAux :: [Int] -> Int -> [(Int, Int)]
posListAux [] _ = []
posListAux (x:xs) c = (x, c) : posListAux xs (c+1)

-- Given a list return its length. Do not use the module length
long :: [Int] -> Int
long [] = 0
long (x:xs) = 1 + long xs

-- Given a list and a number return true if the number is in the list
contains :: [Int] -> Int -> Bool
contains l1 n = length [(True) | x <- l1, x == n] /= 0

-- Given a list of 2-tuple elements return their first elements in a String
firsts :: [(Char, Int)] -> String
firsts [] = ""
firsts ((val1, _):xs) = val1 : firsts xs

-- Given a 2-tuple list return a String where (val1, val2) -> even val2
vals :: [(Char, Int)] -> String
vals [] = ""
vals ((val1, val2):xs) = if even val2 then val1 : vals xs else vals xs

-- Given a list and a number split the list in the number position and return 2-tuple of lists
splitList :: Int -> [Int] -> ([Int], [Int])
splitList _ [] = ([], [])
splitList n l1 = splitListAux n l1 0 ([], [])

splitListAux :: Int -> [Int] -> Int -> ([Int], [Int]) -> ([Int], [Int])
splitListAux _ [] _ (l1, l2) = (l1, l2)
splitListAux n (x:xs) pos (l1, l2) = if pos >= n then splitListAux n xs (pos+1) (l1, l2++[x]) else splitListAux n xs (pos+1) (l1++[x], l2)

-- Given a list and 2 numbers insert a number in the other number (list position)
insertNum :: [Int] -> Int -> Int -> [Int]
insertNum [] new_num _ = [new_num]
insertNum l1 new_num pos = let (ys,zs) = splitAt pos l1 in ys ++ [new_num] ++ zs

-- Given a list return p from odd positions and i from even positions
cod :: [Int] -> String
cod [] = ""
cod xs = codAux xs 0

codAux :: [Int] -> Int -> String
codAux [] _ = ""
codAux (x:xs) c = if even c then "p" ++ codAux xs (c+1) else "i" ++ codAux xs (c+1)

-- Given a list return a list with each element^pos (starting right to left)
listPot :: [Int] -> [Int]
listPot [] = []
listPot (x:xs) = [x^length xs] ++ listPot xs