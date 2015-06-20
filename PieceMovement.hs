module PieceMovement where
import BoardRepresentation

deleteElement :: [a] -> Int -> [a]
deleteElement (x:xs) 0 = xs
deleteElement (x:xs) n = x:(deleteElement xs (n-1))

insertElement :: [a] -> Int -> a -> [a]
insertElement x 0 e      = e:x
insertElement (x:xs) n e = x:(insertElement xs (n-1) e)

insertInPlace :: [a] -> Int -> a -> [a]
insertInPlace (x:xs) 0 e = e:xs
insertInPlace (x:xs) n e = x:(insertInPlace xs (n-1) e)

movePiece :: String -> Position -> Position -> String
movePiece board (sX,sY) (dX,dY) = c
  where
          a = board!!((sY*8)+sX)
          b = insertInPlace board ((sY*9)+sX) ' '
          c = insertInPlace b ((dY*9)+dX) a
