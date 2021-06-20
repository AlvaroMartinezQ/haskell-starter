module Exercise3 where

-- Given the following data definition

data List a = Cons a (List a) | Nil

-- represent using the data type above [1, 2, 3]
li :: Num a => List a
li = (Cons 1 (Cons 2 (Cons 3 Nil)))

-- Given a list defined as above return its length
lenList :: List a -> Int
lenList Nil = 0
lenList (Cons val list) = 1 + lenList list