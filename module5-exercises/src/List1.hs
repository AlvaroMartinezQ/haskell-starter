module List1 where

type Name = String
type Age = Int
type Person = (Name, Age)

-- Given a person defined as above return True if the person can get an age discount
discountAge :: Person -> Bool
discountAge (_, a) = a >= 65

-- Defina a data type Student -> exp number, document number, grade exam number
type ExpNumber = Integer
type Doc = Integer
type FinalGrade = Float
type Student = (ExpNumber, Doc, FinalGrade)
-- Given a Student defined as above determine if he/she has passed a subject
hasPassed :: Student -> Bool
hasPassed (_, _, f) = f >= 5.0
-- Given a Student as above print a message if he/she has passed
strPassed :: Student -> String
strPassed (e, d, f)
		| f >= 5.0 = "Student " ++ show e ++ " with document " ++ show d ++ " has passed with a " ++ show f
		| otherwise = "Student " ++ show e ++ " with document " ++ show d ++ " has not passed with a " ++ show f
-- Given a Student as above return its subject grade. Try it out -> shell> giveGrade (1, 99988888, 6.89)
giveGrade :: Student -> Float
giveGrade (_, _, f) = f
-- Given a list of Students defined as above return their average grades
-- Try it out -> shell> avgs [(11111111, 121, 7), (12678976, 51, 5.3), (03678936, 109, 4.2)]
avgs :: [Student] -> Int
avgs l = avgsAux l 0.0 (length l)

avgsAux :: [Student] -> Float -> Int -> Int
avgsAux [] m a = (round m) `div` a
avgsAux ((_, _, f):xs) m a = avgsAux xs (m + f) a

-- Define a Student type using product types -> exp number, document number, grade exam number
type ProdExp = Integer
type ProdDoc = String
type ProdGrade = Float
data ProStudent = PS ProdExp ProdDoc ProdGrade deriving Show
-- Given a Student as above determine if he/she has passed. Try it out -> shell> prodPassed (PS 121 "11111111-H" 7)
prodPassed :: ProStudent -> Bool
prodPassed (PS _ _ f) = f >= 5.0
-- Given a Student as above print a message if he/she has passed
proStr :: ProStudent -> String
proStr (PS e d f)
		| f >= 5.0 = "Student " ++ show e ++ " with document " ++ show d ++ " has passed with a " ++ show f
		| otherwise = "Student " ++ show e ++ " with document " ++ show d ++ " has not passed with a " ++ show f

-- Define a Student with registry sintax
data StudentReg = SR {
						expNum :: Integer,
						docNum :: String,
						grade :: Float
						} deriving Show
-- Given a list of Students defined as above return their grade average
-- Try it out -> shell> avgReg [(SR 121 "11111111-H" 7), (SR 51 "12678976-P" 5.3), (SR 109 "03678936-T" 4.2)]
avgReg :: [StudentReg] -> Float
avgReg l = avgRegAux l 0.0 0.0

avgRegAux :: [StudentReg] -> Float -> Float -> Float
avgRegAux [] s c = s / c
avgRegAux ((SR _ _ f):xs) s c = avgRegAux xs (s+f) (c+1.0)

-- Given the following data type
data Complex = Com Float Float deriving Show
-- Define a fucntion to return its real part from a Complex data type
realPart :: Complex -> Float
realPart (Com real _) = real
-- Given two Complex data type return its addition of both numbers
sumComplex :: Complex -> Complex -> (Float, Float)
sumComplex (Com r1 i1) (Com r2 i2) = ((r1 + r2), (i1 + i2))

sumComplex' :: Complex -> Complex -> Complex
sumComplex' (Com r1 i1) (Com r2 i2) = (Com (r1 + r2) (i1 + i2))

-- Racional data types
--type
type Numb = Integer
type Deno = Integer
type RacionalType = (Numb, Deno)
-- data
data RacionalData = RacDat Integer Integer deriving Show

-- Given a list return a list with those Racional data types which are equal
-- Racional equal -> (a, b) (c, d), True if a/b == c/d
racionalList :: [RacionalData] -> RacionalData -> [RacionalData]
racionalList [] _ = []
racionalList ((RacDat n1 n2):xs) (RacDat c1 c2)
		| n1 * c2 == c1 * n2 = [(RacDat n1 n2)] ++ racionalList xs (RacDat c1 c2)
		| otherwise = racionalList xs (RacDat c1 c2)