module Exercise2 where

data Categoria = ATP1000 | ATP500 | ATP250 | GrandSlam deriving (Show, Eq)
type Nombre = String
data Torneo = Tor Nombre Categoria
data Temporada = Temp [Torneo]

openAustralia :: Torneo
openAustralia = Tor "Open de Australia" GrandSlam

indianWells :: Torneo
indianWells = Tor "Indian Wells" ATP1000

mutuaMadridOpen :: Torneo
mutuaMadridOpen = Tor "Mutua Madrid Open" ATP1000

wimbledon :: Torneo
wimbledon = Tor "Wimbledon" GrandSlam

temporada2013 :: Temporada
temporada2013 = Temp [openAustralia, indianWells, mutuaMadridOpen, wimbledon]

-- Given a Temporada print all Torneo which are GrandSlam. Use fold functions

getName :: Torneo -> String
getName (Tor tName _) = tName

getCategory :: Torneo -> Categoria
getCategory (Tor _ tCat) = tCat

instance Show Torneo where
	show tor = "Tournament: " ++ show (getName(tor)) ++ " - Category: " ++ show (getCategory(tor))

grandSlams :: Temporada -> [Torneo]
grandSlams (Temp l) = foldr (\t acc -> if (getCategory(t)==GrandSlam) then acc ++ [t] else acc) [] l