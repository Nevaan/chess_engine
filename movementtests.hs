initBoard = unlines [ "RNBQKBNR",
                      "PPPPPPPP",
                      "        ",
                      "        ",
                      "        ",
                      "        ",
                      "pppppppp",
                      "rnbqkbnr"
                    ]

podmien :: [a] -> Int -> a -> [a]
podmien (x:xs) 0 e = e:xs
podmien (x:xs) n e = x:(podmien xs (n-1) e)

replace :: [a] -> Int -> Int -> [a]
replace lista x y      = insertXonY insertYonX
  where insertYonX     = insertElement (deleteElement lista x) x  (lista!!y)
        insertXonY iks = insertElement (deleteElement iks   y) y  (lista!!x)


--usunPion :: Plansza -> (Int,Int) -> Plansza
--usunPion plansza (a,b) = podmien plansza a wiersz
--	where wiersz = usunZWiersza plansza b

deleteElement :: [a] -> Int -> [a]
deleteElement (x:xs) 0 = xs
deleteElement (x:xs) n = x:(deleteElement xs (n-1))

insertElement :: [a] -> Int -> a -> [a]
insertElement x 0 e = e:x
insertElement (x:xs) n e = x:(insertElement xs (n-1) e)

-- changePos :: Plansza -> Pos -> Pos -> Plansza




