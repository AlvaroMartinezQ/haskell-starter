module List2 where

-- Given the following recursive data type
data Expr = V Integer
	|Expr :+: Expr
	|Expr :-: Expr
	|Expr :*: Expr deriving Show

-- Receiving an expression as above, compute the result
calVal :: Expr -> Integer
calVal (V num) = num
calVal (e1 :+: e2) = calVal e1 + calVal e2
calVal (e1 :-: e2) = calVal e1 - calVal e2
calVal (e1 :*: e2) = calVal e1 * calVal e2

-- Receiving an expression as above return the number of operations to process
-- Try it out -> shell> calProc ((V 5) :+: (V 4) :*: (V 2) :+: (V 5) :+: (V 4) :*: (V 2))
calProc :: Expr -> Integer
calProc (V _) = 0
calProc (e1 :+: e2) = 1 + calProc e1 + calProc e2
calProc (e1 :-: e2) = 1 + calProc e1 + calProc e2
calProc (e1 :*: e2) = 1 + calProc e1 + calProc e2

-- Receiving an expression return the number of operands to treat
-- Try it out -> shell> calOps ((V 5) :+: (V 4) :*: (V 2) :+: (V 5) :+: (V 4) :*: (V 2))
calOps :: Expr -> Integer
calOps (V _) = 1
calOps (e1 :+: e2) = calOps e1 + calOps e2
calOps (e1 :-: e2) = calOps e1 + calOps e2
calOps (e1 :*: e2) = calOps e1 + calOps e2

-- Given the following data type
data Tree a = AV | Branch (Tree a) a (Tree a)

-- Receiving a tree and an element return True if the element is contained in the tree
-- Try it out -> 	shell> treeContains 5 (Branch AV 5 (Branch AV 4 AV))
--					shell> treeContains 6 (Branch AV 5 (Branch AV 4 AV))
treeContains :: (Eq a) => a -> Tree a -> Bool
treeContains _ AV = False
treeContains val (Branch l root r) = if val == root then True else treeContains val l || treeContains val r

-- Given a list of trees return the max sum of values inside the trees
-- Try it out -> shell> maxVal [(Branch AV 5 (Branch AV 4 AV)), (Branch AV 15 (Branch AV 4 AV)), (Branch AV 6 (Branch AV 7 AV))]
maxVal :: (Num a, Ord a) => [Tree a] -> a
maxVal t = myMax (maxValAux t) 0

maxValAux :: (Num a) => [Tree a] -> [a]
maxValAux [] = []
maxValAux (x:xs) = [sumTree x] ++ maxValAux xs

sumTree :: (Num a) => Tree a -> a
sumTree (Branch AV root AV) = root
sumTree (Branch l root AV) = root + sumTree l
sumTree (Branch AV root r) = root + sumTree r
sumTree (Branch l root r) = root + sumTree l + sumTree r

myMax :: (Ord a) => [a] -> a -> a
myMax [] val = val
myMax (x:xs) val = if val < x then myMax xs x else myMax xs val

-- Receiving a tree return a string of itself
printTree :: (Show a) => (Tree a) -> String
printTree (Branch AV root AV) = "(" ++ show root ++ ")"
printTree (Branch l root AV) = "(" ++ show (printTree l) ++ "|-" ++ show root ++ "-|AV)"
printTree (Branch AV root r) = "(AV|-" ++ show root ++ "-|" ++ show (printTree r) ++")"
printTree (Branch l root r) = "(" ++ show (printTree l) ++ "|-" ++ show root ++ "-|" ++ show (printTree r) ++ ")"