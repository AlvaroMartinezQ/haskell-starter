module List3 where

-- Given a polimorfic function receiving 2 lists return a 3-tuple list (1 elem 1st list, 1 elem 2nd list, 1 elem 3rd list)
mix :: [a] -> [b] -> [(a, b, b)]
mix [] _ = []
mix _ [] = []
mix (x:xs) (y:z:ys) = [(x, y, z)] ++ mix xs ys

-- Given a list insert a received element in the last position
lastPosIn :: [a] -> a -> [a]
lastPosIn l el = l ++ [el]