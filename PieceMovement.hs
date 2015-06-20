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
