module List1 where

-- Given 3 numbers the function returns True if they're in order
inOrder :: Int -> Int -> Int -> Bool
inOrder x y z = if (x < y) && (y < z) then True else False