module BoardRepresentation where

data Colour = Black | White deriving Show
data Figure = King | Queen | Rook | Bishop | Knight | Pawn deriving Show
data ColouredFigure = ColouredFigure Colour Figure deriving Show

type Square = Maybe ColouredFigure 
type Board = [[Square]]
type Position = (Int, Int) -- starting with upper left corner, coordinates are (x,y)

initBoard = unlines [ "RNBQKBNR",
                      "PPPPPPPP",
                      "        ",
                      "        ",
                      "        ",
                      "        ",
                      "pppppppp",
                      "rnbqkbnr"
                    ]
