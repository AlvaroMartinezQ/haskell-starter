module List2 where

-- Change the following expression using lambda expression
	{-
			d :: Int -> Int
			d x = x + x
	-}
	
d = (\x -> x + x)

-- Given a list return the addition of all elements * 2
-- Non final recursion
elevElemsAdd :: [Int] -> Int
elevElemsAdd [] = 0
elevElemsAdd (x:xs) = x*2 + elevElemsAdd xs

-- Final recursion
elevElemsAdd' :: [Int] -> Int
elevElemsAdd' l = elevElemsAddAux l 0

elevElemsAddAux :: [Int] -> Int -> Int
elevElemsAddAux [] c = c
elevElemsAddAux (x:xs) c = elevElemsAddAux xs (c + (x*2))

-- foldr
elevElemsAdd'' :: [Int] -> Int
elevElemsAdd'' l = foldr(\el acc -> el + acc) 0 l

-- Given a list return an addition of all even elements ^ 2
cuad :: [Int] -> Int
cuad l = foldr(\el acc -> if even el then acc + (el ^ 2) else acc) 0 l

cuad' :: [Int] -> Int
cuad' l = foldl(\acc el -> if even el then acc + (el ^ 2) else acc) 0 l

cuad'' :: [Int] -> Int
cuad'' l = sum (map (\x -> x ^ 2) (filter even l))

-- Given a list and a number remove all number occurrences from the list
remOcc :: [Int] -> Int -> [Int]
remOcc l n = foldr(\el acc -> if el == n then acc else acc ++ [el]) [] l

remOcc' :: [Int] -> Int -> [Int]
remOcc' l n = foldl(\acc el -> if el == n then acc else acc ++ [el]) [] l

-- Given a list return another one with all prime numbers from the original
		--
divis :: Int -> [Int]
divis number = [x | x <- [1..number], number `rem` x == 0]

isPrime :: Int -> Bool
isPrime n = length(divis n) == 2
		--
dropPrimes :: [Int] -> [Int]
dropPrimes [] = []
dropPrimes (x:xs) = if isPrime x then [x] ++ dropPrimes xs else dropPrimes xs