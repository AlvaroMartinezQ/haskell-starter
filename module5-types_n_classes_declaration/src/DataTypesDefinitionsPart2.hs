module DataTypesDefinitionsPart2 where

-- Define a Matrix data type
data Matrix = M [[Int]]

m1 :: Matrix
m1 = M [[1,2,3,8],[3,4,5,6],[1,5,1,7]]

-- Given a matrix return the number of rows and cols
matrixSize :: Matrix -> (Int, Int)
matrixSize (M []) = (0, 0)
matrixSize (M rows) = (length rows, length (head rows))

-- Define a function that returns working days
data Day = Mon | Tue | Wen | Thu | Fri | Sat | Sun deriving Show

working :: [Day]
working = [Mon, Tue, Wen, Thu, Fri]

-- Given the following data type make a function that returns the number of expresions in an operation
data Expr = V Integer
	|Expr :+: Expr
	|Expr :-: Expr
	|Expr :*: Expr deriving Show
	
numberOps :: Expr -> Int
numberOps (V _) = 0
numberOps (e1 :+: e2) = 1 + numberOps e1 + numberOps e2
numberOps (e1 :-: e2) = 1 + numberOps e1 + numberOps e2
numberOps (e1 :*: e2) = 1 + numberOps e1 + numberOps e2

-- Given the following data type determine if a node is a leaf
data Tree a = AV | Stick (Tree a) a (Tree a)

isLeaf :: Tree a -> Bool
isLeaf (Stick AV _ AV) = True
isLeaf _ = False

-- Define a function that returns the number of leafs
leafs :: Tree a -> [a]
leafs AV = []
leafs (Stick AV h AV) = [h]
leafs (Stick l h r) = leafs l ++ leafs r

numLeafs :: Tree a -> Int
numLeafs t = length (leafs t)

-- Given a list and a position (integer) return the value in that position of the list, if exists
-- Try it out -> 	shell> positionList [1,2,3,4,5,6] 4
-- 					shell> positionList [1,2,3,4,5,6] 6 
positionList :: [Int] -> Int -> Maybe Int
positionList l num = postionListAux l num 0

postionListAux :: [Int] -> Int -> Int -> Maybe Int
postionListAux [] _ _ = Nothing
postionListAux (x:xs) num con = if num == x then Just con else postionListAux xs num (con + 1)

-- Given 2 trees, defined as exercises above, determine if both trees are equal
sameTree :: (Eq a) => Tree a -> Tree a -> Bool
sameTree AV AV = True
sameTree AV _ = False
sameTree _ AV = False
sameTree (Stick l1 a1 r1) (Stick l2 a2 r2) = a1 == a2 && sameTree l1 l2 && sameTree r1 r2 -- Don't forget the (Eq => a) in the fucntion

-- Given a list of elements return the smaller one
smaller :: Ord a => [a] -> Maybe a
smaller [] = Nothing
smaller [x] = Just x
smaller (x:y:xs) = smaller ((min x y):xs)

-- Given a number and a list of 2-tuples return (if exists) the tuple partner, for example:
-- number: 5 | list: [(7,2),(8,10),(5,11),(3,4)] -> returns 11
mates :: Eq a => a -> [(a, b)] -> Maybe b
mates _ [] = Nothing
mates val ((x, y):xs) = if x == val then Just y else mates val xs