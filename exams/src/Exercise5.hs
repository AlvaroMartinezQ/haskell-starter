module Exercise5 where

-- Given the following data type
data Arbol a = AV | Rama (Arbol a) a (Arbol a)

-- Given an Arbol return 2 lists -> 1 with no repeated elements and another with the repeated ones
repes :: Arbol a -> ([a], [a])
repes AV = ([], [])
repes t = repesAux t [] []

repesAux :: Arbol a -> [a] -> [a] -> ([a], [a]) -- Tree - repetitions list - non repetitions list
repesAux AV lr lu = (lr, lu)
repesAux (Rama left a right) lr lu = if (inList a lu) then repesAux left (addToList lr a) (removeFromList lu a) ++ repesAux right (addToList lr a) (removeFromList lu a) else repesAux left lr (addToList lu a) ++ repesAux right lr (addToList lu a)

inList :: (Eq a) => a -> [a] -> Bool
inList _ [] = False
inList val (x:xs) = if val == x then True else inList val xs

removeFromList :: (Eq a) => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList val (x:xs) = if x == val then removeFromList val xs else [x] ++ removeFromList val xs

addToList :: a -> [a] -> [a]
addToList val l = l ++ [val]
