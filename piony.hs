data Kolor = Czarny | Bialy deriving Show

data BFigura = Krol | Dama | Goniec | Wieza | Skoczek | Pion deriving Show

data Figura = Figura Kolor BFigura deriving Show

type Pole = Maybe Figura

type Plansza = [[Pole]]

type Pos = (Int, Int)

iplansza = ["KDGSPK"
	   ,"pppppppp"
	   ,"        "]

pokazF :: Figura -> Char
pokazF (Figura Czarny Krol) = 'K'
pokazF (Figura Czarny Dama) = 'D'
pokazF (Figura Czarny Wieza) = 'W'
pokazF (Figura Czarny Goniec) = 'G'
pokazF (Figura Czarny Skoczek) = 'S'
pokazF (Figura Czarny Pion) = 'P'


pokazF (Figura Bialy Krol) = 'k'
pokazF (Figura Bialy  Dama) = 'd'
pokazF (Figura Bialy  Goniec) = 'g'
pokazF (Figura Bialy Wieza) = 'w'
pokazF (Figura Bialy  Skoczek) = 's'
pokazF (Figura Bialy   Pion) = 'p'


czytajF :: Char -> Figura
czytajF 'K'= (Figura Czarny Krol)
czytajF 'D'= (Figura Czarny Dama)
czytajF 'G'= (Figura Czarny Goniec)
czytajF 'W' = (Figura Czarny Wieza)
czytajF 'S'= (Figura Czarny Skoczek)
czytajF 'P'= (Figura Czarny Pion)

czytajF 'k'= (Figura Bialy Krol)
czytajF 'd'= (Figura Bialy Dama)
czytajF 'g'= (Figura Bialy Goniec)
czytajF 'w' = (Figura Bialy Wieza)
czytajF 's'= (Figura Bialy Skoczek)
czytajF 'p'= (Figura Bialy Pion)

pokazPole :: Pole -> Char
pokazPole Nothing = ' '
pokazPole (Just p) = pokazF p

czytajPole :: Char -> Pole
czytajPole ' ' = Nothing
czytajPole c = Just(czytajF c)

czytajPlansze :: String -> Plansza
czytajPlansze = map czytajRzad . lines
	where czytajRzad =  map czytajPole


pokazPlansze :: Plansza -> String
pokazPlansze = unlines . map pokazRzad
	where pokazRzad = map pokazPole


podmien :: [a] -> Int -> a -> [a]
podmien (x:xs) 0 e = e:xs
podmien (x:xs) n e = x:(podmien xs (n-1) e)

usunZWiersza :: [Maybe a]  -> Int -> [Maybe a]
usunZWiersza plansza indeks = podmien plansza indeks Nothing 


--usunPion :: Plansza -> (Int,Int) -> Plansza
--usunPion plansza (a,b) = podmien plansza a wiersz
--	where wiersz = usunZWiersza plansza b

usunElement :: [a] -> Int -> [a]
usunElement (x:xs) 0 = xs
usunElement (x:xs) n = x:(usunElement xs (n-1))


wstawElement :: [a] -> Int -> a -> [a]
wstawElement x 0 e = e:x
wstawElement (x:xs) n e = x:(wstawElement xs (n-1) e)


-- changePos :: Plansza -> Pos -> Pos -> Plansza




