module Exercise4 where

-- Accident -> Airport name, deaths, hardly wounded, wounded
-- Incident -> Airport name, hardly wounded, wounded
data Suce = Accident String Int Int Int | Incident String Int Int 

-- Impletent a function to compare 2 data types Suce
instance Eq Suce where
	(Accident n d hw w) == (Accident n' d' hw' w') = n == n' && d == d' && hw == hw' && w == w'
	(Incident n hw w) == (Incident n' hw' w') = n == n' && hw == hw' && w == w'
	_ == _ = False