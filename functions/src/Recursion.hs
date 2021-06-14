module Recursion where

-- Example of recursion in haskell
fact :: Int -> Int
fact x = if x == 0 then 1 else x * fact (x-1)

fibo :: Int -> Int
-- fibo x = if x == 0 || x == 1 then 1 else fibo (x-1) + fibo(x-2) -- Next 3 lines are the same with this one
fibo 0 = 1
fibo 1 = 1
fibo x = fibo (x-1) + fibo (x-2)

-- map function | Iterates through each element of the list
mapIt :: [Int] -> [Int]
mapIt l1 = map (*2) l1

-- filter function | filters all elements in a list which match a given condition
filterIt :: [Int] -> [Int]
filterIt l1 = filter even l1

-- all function | verifies that all the elements in a list match the given condition
allIt :: [Int] -> Bool
allIt l1 = all even l1

-- any function | verifies if there's any value with the given condition
anyIt :: [Int] -> Bool
anyIt l1 = any even l1

-- fold functions | notice the acumulator and element are in inverse order for foldl and foldr
foldlIt :: [Int] -> Int -> Int
foldlIt l1 s = foldl (\acc e -> if s == e then acc+1 else acc) 0 l1

foldrIt:: [Int] -> Int -> Int
foldrIt l1 s = foldr (\e acc -> if s == e then acc+1 else acc) 0 l1

-- Polimorfism
lon :: [a] -> Int
lon [] = 0
lon (x:xs) = 1 + lon xs