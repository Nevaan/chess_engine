data Colour = Black | White deriving Show

data Figure = King | Queen | Rook | Bishop | Knight | Pawn deriving Show

data ColouredFigure = ColouredFigure Colour Figure deriving Show

type Square = Maybe ColouredFigure

type Board = [[Square]]

-- starting with upper left corner, coordinates are (x,y)
type Position = (Int, Int)

initBoard = unlines [ "RNBQKBNR",
                      "PPPPPPPP",
                      "        ",
                      "        ",
                      "        ",
                      "        ",
                      "pppppppp",
                      "rnbqkbnr"
                    ]


showFigure :: ColouredFigure -> Char
showFigure (ColouredFigure Black King)   = 'K'
showFigure (ColouredFigure Black Queen)  = 'Q'
showFigure (ColouredFigure Black Rook)   = 'R'
showFigure (ColouredFigure Black Bishop) = 'B'
showFigure (ColouredFigure Black Knight) = 'N'
showFigure (ColouredFigure Black Pawn)   = 'P'

showFigure (ColouredFigure White King)   = 'k'
showFigure (ColouredFigure White Queen)  = 'q'
showFigure (ColouredFigure White Rook)   = 'r'
showFigure (ColouredFigure White Bishop) = 'b'
showFigure (ColouredFigure White Knight) = 'n'
showFigure (ColouredFigure White Pawn)   = 'p'


readFigure :: Char -> ColouredFigure
readFigure 'K' = (ColouredFigure Black King)
readFigure 'Q' = (ColouredFigure Black Queen)
readFigure 'R' = (ColouredFigure Black Rook)
readFigure 'B' = (ColouredFigure Black Bishop)
readFigure 'N' = (ColouredFigure Black Knight)
readFigure 'P' = (ColouredFigure Black Pawn)

readFigure 'k' = (ColouredFigure White King)
readFigure 'q' = (ColouredFigure White Queen)
readFigure 'r' = (ColouredFigure White Rook)
readFigure 'b' = (ColouredFigure White Bishop)
readFigure 'n' = (ColouredFigure White Knight)
readFigure 'p' = (ColouredFigure White Pawn)

showSquare :: Square -> Char
showSquare Nothing     = ' '
showSquare (Just p) = showFigure p

readSquare :: Char -> Square
readSquare ' ' = Nothing
readSquare c   = Just (readFigure c)

readBoard :: String -> Board
readBoard = map readLine . lines
  where readLine = map readSquare

showBoard :: Board -> String
showBoard = unlines . map showLine
  where showLine = map showSquare

deleteElement :: [a] -> Int -> [a]
deleteElement (x:xs) 0 = xs
deleteElement (x:xs) n = x:(deleteElement xs (n-1))

insertElement :: [a] -> Int -> a -> [a]
insertElement x 0 e      = e:x
insertElement (x:xs) n e = x:(insertElement xs (n-1) e)


insertInPlace :: [a] -> Int -> a -> [a]
insertInPlace (x:xs) 0 e = e:xs
insertInPlace (x:xs) n e = x:(insertInPlace xs (n-1) e)

swap :: [a] -> Int -> Int -> [a]
swap lista x y = insertXonY insertYonX
  where
        insertYonX     = insertElement (deleteElement lista x) x  (lista!!y)
        insertXonY iks = insertElement (deleteElement iks   y) y  (lista!!x)


movePiece :: String -> Position -> Position -> String
movePiece board (sourceX,sourceY) (destinationX,destinationY) = unlines insertReadyDestRow
  where
    linedBoard                      = lines board
    sourceRow                       = linedBoard!!sourceY
    sourcePiece                     = sourceRow!!sourceX
    removeSourcePiece               = deleteElement sourceRow sourceX
    insertBlank                     = insertElement removeSourcePiece sourceX ' '
    insertBlankedToBoard            = insertInPlace linedBoard sourceY insertBlank
    destinationRow                  = insertBlankedToBoard!!destinationY
    removeDestinationPiece          = deleteElement destinationRow destinationX
    insertOnDestination             = insertElement removeDestinationPiece destinationX sourcePiece
    insertReadyDestRow              = insertInPlace insertBlankedToBoard destinationY insertOnDestination


possibleMoves :: Figure -> Position -> [(Int,Int)]
possibleMoves King   (currentX, currentY)  = [(x,y)|x <-[currentX+1,currentX,currentX-1], y <-[currentY+1,currentY,currentY-1],(x,y)/=(currentX,currentY),x>=0,y>=0,x<=7,y<=7]
possibleMoves Queen  (currentX, currentY)  = (possibleMoves Rook (currentX,currentY))++(possibleMoves Bishop (currentX,currentY))
possibleMoves Rook   (currentX, currentY)  = vertical++horizontal
  where vertical   = [(currentX+x,currentY)|x <-[-7..7],(currentX+x)>=0,(currentX+x)<=7,(currentX+x,currentY)/=(currentX,currentY)]
        horizontal = [(currentX,currentY+y)|y <-[-7..7],(currentY+y)>=0,(currentY+y)<=7,(currentX,currentY+y)/=(currentX,currentY)]
possibleMoves Bishop (currentX, currentY)  = backslashDiagonal++slashDiagonal
  where
        backslashDiagonal = [(currentX+x,currentY+x)|x <- [-7..7],(currentX+x)>=0, (currentX+x)<=7,(currentY+x)>=0, (currentY+x)<=7,x/=0]
        slashDiagonal     = [(currentX+x,currentY-x)|x <- [-7..7],(currentX+x)>=0, (currentX+x)<=7,(currentY-x)>=0, (currentY-x)<=7,x/=0]
possibleMoves Knight (currentX, currentY)  = [(currentX+x,currentY+y)|x<-[-2,-1,1,2],y<-[-2,-1,1,2],(currentX+x,currentY+y)/=(currentX+x,currentY+x),(currentX+x,currentY+y)/=(currentX-y,currentY+y),
                                              (currentX+x)>=0,(currentX+x)<=7,(currentY+y)>=0,(currentY+y)<=7]
--possibleMoves Pawn   (currentX, currentY)  =

