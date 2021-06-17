module List1 where

type Name = String
type Age = Int
type Person = (Name, Age)

-- Given a person defined as above return True if the person can get an age discount
discountAge :: Person -> Bool
discountAge (_, a) = a >= 65