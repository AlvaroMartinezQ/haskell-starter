module DataTypesDefinitions where

-- type ------------------------------------------------------------------------
type Name = String
type Age = Integer
type Person = (Name, Age)

-- Try it out
-- samePerson ("alvaro", 22) ("pepe", 10)
-- samePerson ("alvaro", 22) ("alvaro", 21)
samePerson :: Person -> Person -> Bool
samePerson (n1, _) (n2, _) = n1 == n2

data Person' = Pers Name Age deriving Show -- Pers is the constructor

-- Create a person
alvaro :: Person'
alvaro = Pers "alvaro" 22

-- Functions to treat this data | Try it out: shell> isYoung alvaro
isYoung :: Person' -> Bool
isYoung (Pers _ age) = age <= 25

-- Declare the data directly
data Person'' = P String String Integer deriving Show
pepe :: Person''
pepe = P "pepe" "fuentes" 29

-- data ------------------------------------------------------------------------
data Color = Red | Yellow | Blue deriving Show
data Intensity = Hot | Medium | Cold deriving Show

example :: Color -> Intensity
example Red = Hot
example Yellow = Medium
example _ = Cold

data LetterInteger = Letter Char | Inte Integer deriving Show

-- Another way of defining data types ------------------------------------------
data NewPerson = NP {
						name :: String,
						surname1 :: String,
						surname2 :: String,
						age :: Integer
					} deriving Show
juan :: NewPerson
juan = NP {
			name = "Juan",
			surname1 = "pepito",
			surname2 = "sanchez",
			age = 45
			}
			
-- recursive types -------------------------------------------------------------
data Expr = Value Integer
				| Expr :+: Expr
				| Expr :-: Expr
				| Expr :*: Expr deriving Show

ex :: Expr
ex = Value 5 :+: Value 4

-- Polimorfic types
data Two a = ATow a a deriving Show

-- Binary trees
data Tree a = AV | Stick (Tree a) a (Tree a)

leaves :: Tree a -> [a]
leaves (Stick AV a AV) = [a]
leaves (Stick l a r) = leaves l ++ leaves r

-- Maybe Nothing Just
divi :: Int -> Int -> Maybe Int
divi _ 0 = Nothing
divi n1 n2 = Just (n1 `div` n2)