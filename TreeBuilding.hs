module TreeBuilding where
import PieceMovement
import BoardRepresentation
import BoardDisplay

figureValue :: ColouredFigure -> Int
figureValue (ColouredFigure _  Pawn  )  = 1
figureValue (ColouredFigure _  Knight)  = 3
figureValue (ColouredFigure _  Bishop)  = 3
figureValue (ColouredFigure _  Rook  )  = 5
figureValue (ColouredFigure _  Queen )  = 9
figureValue (ColouredFigure _  King )  = 0

figureValue' :: Figure -> Int
figureValue' Pawn   = 1
figureValue' Knight = 3
figureValue' Bishop = 3
figureValue' Rook   = 5
figureValue' Queen  = 9

countActualFiguresValue:: String -> (Int, Int)  -- (White,Black)
countActualFiguresValue board = (x, y)
  where
      x = sum (map figureValue [readFigure x| x <- board, elem x ['a'..'z']])
      y = sum (map figureValue [readFigure x| x <- board, elem x ['A'..'Z']])


possibleMoves :: ColouredFigure -> Position -> [Position]
possibleMoves (ColouredFigure _ King)   (currentX, currentY)  = [(x,y)|x <-[currentX+1,currentX,currentX-1], y <-[currentY+1,currentY,currentY-1],(x,y)/=(currentX,currentY),x>=0,y>=0,x<=7,y<=7]
possibleMoves (ColouredFigure _ Queen)  (currentX, currentY)  = rookMoves++bishopMoves
  where rookMoves   = possibleMoves (ColouredFigure White Rook) (currentX, currentY)
        bishopMoves = possibleMoves (ColouredFigure White Bishop) (currentX, currentY)

possibleMoves (ColouredFigure _ Rook)   (currentX, currentY)  = vertical++horizontal
  where vertical   = [(currentX+x,currentY)|x <-[-7..7],(currentX+x)>=0,(currentX+x)<=7,(currentX+x,currentY)/=(currentX,currentY)]
        horizontal = [(currentX,currentY+y)|y <-[-7..7],(currentY+y)>=0,(currentY+y)<=7,(currentX,currentY+y)/=(currentX,currentY)]

possibleMoves (ColouredFigure _ Bishop) (currentX, currentY)  = backslashDiagonal++slashDiagonal
  where backslashDiagonal = [(currentX+x,currentY+x)|x <- [-7..7],(currentX+x)>=0, (currentX+x)<=7,(currentY+x)>=0, (currentY+x)<=7,x/=0]
        slashDiagonal     = [(currentX+x,currentY-x)|x <- [-7..7],(currentX+x)>=0, (currentX+x)<=7,(currentY-x)>=0, (currentY-x)<=7,x/=0]

possibleMoves (ColouredFigure _ Knight) (currentX, currentY)  = [(currentX+x,currentY+y)|x<-[-2,-1,1,2],y<-[-2,-1,1,2],(currentX+x,currentY+y)/=(currentX+x,currentY+x),(currentX+x,currentY+y)/=(currentX-y,currentY+y),
                  (currentX+x)>=0,(currentX+x)<=7,(currentY+y)>=0,(currentY+y)<=7]

possibleMoves (ColouredFigure Black Pawn) (currentX, 1)        = [(currentX,2),(currentX,3)]++[(currentX+x,2)|x<-[-1,1],(currentX+x)<=7,(currentX+x)>=0]
possibleMoves (ColouredFigure Black Pawn) (currentX, currentY) = [(currentX,currentY+1)|(currentY+1)<=7]++diagonal
  where diagonal = [(currentX+x,currentY+1)|x<-[-1,1],(currentX+x)>=0, (currentX+x)<=7,(currentY+1)<=7]

possibleMoves (ColouredFigure White Pawn) (currentX, 6)        = [(currentX,5),(currentX,4)]++[(currentX+x,5)|x<-[-1,1],(currentX+x)<=7,(currentX+x)>=0]
possibleMoves (ColouredFigure White Pawn) (currentX,currentY)  =  [(currentX,currentY-1)|(currentY+1)>=0]++diagonal
  where diagonal = [(currentX+x,currentY-1)|x<-[-1,1],(currentX+x)>=0, (currentX+x)<=7,(currentY-1)>=0]


-- nie dokonczona funkcja
allMoves:: String -> Colour -> [((Int,Char),[Position])]
allMoves board colour = movesPlusFigures
    where
        figuresList = if (colour == White) then
                                           [(x,y)|(x,y)<-(zip [0..] board),elem y ['a'..'z']]
                                          else
                                           [(x,y)|(x,y)<-(zip [0..] board),elem y ['A'..'Z']]
        moves = [possibleMoves (readFigure y) ((mod x 9),(div x 9))  |(x,y)<-figuresList]
        movesPlusFigures = zip figuresList moves
