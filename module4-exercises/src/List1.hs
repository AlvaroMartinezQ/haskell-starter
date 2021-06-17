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