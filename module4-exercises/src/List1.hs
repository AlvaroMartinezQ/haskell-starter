module List1 where

-- Given an Integer and a list delete from the list all multiples of the Integer
-- Lists comprehenssion
crib :: Int -> [Int] -> [Int]
crib n l = [x | x <- l, x `rem` n /= 0]

-- Not final recursion
crib' :: Int -> [Int] -> [Int]
crib' _ [] = []
crib' n (x:xs) = if x `rem` n /= 0 then [x] ++ crib' n xs else crib' n xs

-- Final recursion
crib'' :: Int -> [Int] -> [Int]
crib'' n l = cribAux n l []

cribAux :: Int -> [Int] -> [Int] -> [Int]
cribAux _ [] new = new
cribAux n (x:xs) new = if x `rem` n /= 0 then cribAux n xs (new ++ [x]) else cribAux n xs new

-- Given a list return an Integer with the number of 0s sequences in the list
ceros :: [Int] -> Int
ceros [] = 0
ceros (x:xs)
			| x == 0 = 1 + ceros (dropWhile (==0) xs)
			| otherwise = ceros xs
			
-- Given a list return 2 lists one with all elements and another with those repeated elements
split :: [Int] -> ([Int], [Int])
split xs = splitAux xs [] []

splitAux :: [Int] -> [Int] -> [Int] -> ([Int], [Int])
splitAux [] l1 l2 = (l1, l2)
splitAux (x:xs) l1 l2
	| inList x xs = if inList x l2 then splitAux xs l1 l2 else splitAux xs l1 (l2 ++ [x])
	| otherwise = splitAux xs (l1 ++ [x]) l2
	
inList :: Int -> [Int] -> Bool
inList _ [] = False
inList n (x:xs) = n == x || inList n xs

-- Given a list and a number (n) remove all last n elements
removeElems :: [Int] -> Int -> [Int]
removeElems l n = removeElemsAux l [] n (length l)

removeElemsAux :: [Int] -> [Int] -> Int -> Int -> [Int]
removeElemsAux [] newl _ _ = newl
removeElemsAux (x:xs) newl n c = if n == c then newl else removeElemsAux xs (newl ++ [x]) n (c-1)

-- Given a list return True if they're in order, min to max
minToMax :: [Int] -> Bool
minToMax [] = True
minToMax (x:[]) = True
minToMax (x:y:xs) = if x<y then minToMax ([y]++xs) else False