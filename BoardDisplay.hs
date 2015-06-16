module BoardDisplay where
import BoardRepresentation

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
