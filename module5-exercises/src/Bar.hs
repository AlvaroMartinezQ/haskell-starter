module Bar where

data Table = T {
				tableId :: Int,
				tableChairs :: Int
				} deriving Show
				
data Ocupation = O {
					free :: [Table],
					notFree :: [Table]
					} deriving Show
					
printTables :: Ocupation -> String
printTables (O fr nf) = "Free tables: " ++ printTable fr ++ ". Not free: " ++ printTable nf

printTable :: [Table] -> String
printTable [] = ""
printTable ((T i c):xs) = "Table: " ++ show i ++ ", chairs: " ++ show c ++ " | " ++ printTable xs

insertTable :: Ocupation -> Table -> Ocupation
insertTable (O fr nf) tab = O (fr ++ [tab]) nf 

giveTable :: Ocupation -> Int -> Maybe Table
giveTable (O fr nf) n_pers = giveTableAux fr (T 0 100) n_pers -- Set the initial persons as 100 as no table will have that seatss

giveTableAux :: [Table] -> Table -> Int -> Maybe Table -- [Free tables] [Last best table found] [nº of persons]
giveTableAux [] (T i c) _ = if i == 0 then Nothing else Just (T i c)
giveTableAux ((T i c):xs) (T i' c') n_pers = if c >= n_pers && c < c' then giveTableAux xs (T i c) n_pers else giveTableAux xs (T i' c') n_pers

table1 :: Table
table1 = T 1 5

table2 :: Table
table2 = T 2 5

table3 :: Table
table3 = T 3 15

table4 :: Table
table4 = T 4 10

ocupation1 :: Ocupation
ocupation1 = O [table1, table3] [table2, table4]