module HandleFiles where

import TicToe
import Data.List.Split
import Data.Char
import Debug.Trace
import Data.List


fileToGameState :: String -> IO (Maybe GameState)
fileToGameState fileName = do
    file <- readFile fileName
    return (parseFile file)
    
parseFile :: String -> Maybe GameState
parseFile file = 
       do let (player:kMove:boards) = lines file
          mBoard <- stringToMasterBoard boards
          mPlayer <- stringToPlayer player 
          mKMove <- (stringToKMove kMove)
          Just (mBoard,mPlayer,mKMove)


stringToPlayer :: String -> Maybe Player 
stringToPlayer "P1" = Just P1
stringToPlayer "P2" = Just P2
stringToPlayer _ = Nothing

stringToKMove :: String -> Maybe KindOfMove
stringToKMove kMove = 
        case (splitOn "," kMove) of
          ["free"]      -> Just Free
          [[row],[col]] -> if validInd row && validInd col 
                            then Just $ Forced ((digitToInt row) -1, (digitToInt col) -1)
                            else Nothing
          _             -> Nothing
        where validInd i = if ( i >= '1' && i <= '3') then True else False
      
stringToMasterBoard :: [String] -> Maybe (Board SubBoard)
stringToMasterBoard mBoard  = 
        let row1 = (take 3 mBoard)
            row2 = (take 3 (drop 3 mBoard))
            row3 = (drop 6 mBoard)
            mRow1 = sequence $ map stringToBoard row1
            mRow2 = sequence $ map stringToBoard row2
            mRow3 = sequence $ map stringToBoard row3
            mBoards = mRow1:mRow2:mRow3:[]
        in  sequence mBoards

stringToBoard :: String -> Maybe SubBoard
stringToBoard board = 
        let rows = words board
            mRows = map stringToRow rows
        in  sequence mRows

stringToRow :: String -> Maybe (Row Piece)
stringToRow row =
        let useful = splitOn "," row
            comp  =  map stringToPiece useful
        in  sequence comp


stringToPiece :: String -> Maybe Piece
stringToPiece "e" = Just Empty
stringToPiece "P1" = Just $ Full P1
stringToPiece "P2" = Just $ Full P2
stringToPiece _   = Nothing


writeToFile ::  GameState -> String -> IO ()
writeToFile game filename = 
    let  msg = stateToString game 
     in do writeFile filename msg

stateToString :: GameState ->  String
stateToString (masterBoard,player,kMove) = 
    let sBoard  = mBoardToString masterBoard
        sPlayer = playerToString player
        skMove  = kMoveToString kMove
        in  sPlayer ++ "\n" ++ skMove ++ "\n" ++ sBoard

playerToString :: Player -> String
playerToString player = case player of
        P1 -> "P1"
        P2 -> "P2"

kMoveToString :: KindOfMove -> String
kMoveToString kMove = case kMove of
        Free -> "free"
        (Forced (x,y)) -> ((show (x+1))++ "," ++ (show (y+1)))

mBoardToString :: MasterBoard -> String
mBoardToString mBoard = 
        let row1 = head mBoard
            row2 = head (tail mBoard)
            row3 = last mBoard
            srow1 = concat $ map boardToString row1
            srow2 = concat $ map boardToString row2
            srow3 = concat $ map boardToString row3
        in srow1 ++ srow2 ++ srow3


boardToString :: SubBoard -> String
boardToString board = 
        let rows =  map rowToString board
        in (intercalate " " rows) ++ "\n"   


rowToString :: Row Piece -> String
rowToString row = 
    let part = map pieceToString row
    in (intercalate "," part)


pieceToString :: Piece -> String
pieceToString piece = case piece of
    (Full P1) -> "P1"
    (Full P2) -> "P2"
    otherwise -> "e"
