
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




