module TicToe where
import Data.Maybe
--perfectly balanced, as all things should be.
--


--            no piece,  players
data Piece = Empty    | Full Player deriving (Show, Eq)


-- a Row has 3 columns in it
type Row a = [a]

-- board will have 3 rows 
type Board a = [Row a]

{-
  instances of board we will use:
  Master board -> the overall game board
  sub board -> 3x3 game of TicTacToe
-}
-- who is currently playing
data Player = P1 | P2 deriving (Show, Eq)

-- Location at a board
type Loc = (Int,Int)

-- 3x3 tic tac toe game
type SubBoard = Board Piece

-- overall board where game take place
type MasterBoard = Board SubBoard

-- Tie = hard tie, fully filled board with no winner
-- Progress = game still uccoring no winner yet, empty spaces
-- player, that player has won the board, may still have empty spaces or not
data Outcome = Tie | Win Player deriving (Show,Eq)
data Status = Ongoing | Over Outcome deriving (Show, Eq)


-- first loc is what board to play on in masterboard, second is where to play in subboard
type Move = (Loc,Loc)

data KindOfMove = Free | Forced Loc deriving (Show, Eq)


--               MasterBoard,  current turn player ,  coordinates for next play
type GameState = (MasterBoard, Player, KindOfMove)

-- represents a move, with Forced holding what spot you play on a normal board 
--                         Free Holding what board you play on and what spot to ply in
--

--                            == MileStone 1 ==
--
--The following are used for testing and in Main
practiceBoard = [[Empty, Full P2, Full P2],
                 [Empty, Full P1, Full P1],
                 [Empty, Empty, Full P2]]


emptyBoard = replicate 3 (replicate 3 Empty)
emptyMaster = replicate 3 (replicate 3 emptyBoard)
p1Board = replicate 3 ( replicate 3 (Full P1))

p2Board = replicate 3 (replicate 3 (Full P2))

pMaster = [[p1Board, p2Board, p1Board],
           [p2Board, practiceBoard, p2Board],
           [p1Board, p2Board, p1Board]] 

--firstMaster = boardInsert pMaster (1,1) practiceBoard


firstGameState = (emptyMaster, P1, Free) --         

--
-- calculated at end of turn, will check MasterBaord to check for state of game
overallStatus :: GameState -> Status
overallStatus (masterBoard, player, kMove) = 
  let vBoard = [ map boardStatus boards | boards <- masterBoard]
  in vBoardStatus vBoard

boardIndex :: Board a -> Loc -> Maybe a
boardIndex board (x, y) = if x < 0 || x > 2 || y < 0 || y > 2 then Nothing
                          else Just $ (board!!y)!!x
  
boardInsert :: Board a -> Loc -> a -> Maybe (Board a)
boardInsert board (x,y) obj = 
  let aux :: [b] -> Int -> Int -> b -> [b]
      aux [] _ _ _ = []
      aux (p:ps) goal spot toInsert = if (goal == spot) then toInsert:(aux ps goal (spot+1) toInsert) else p:(aux ps goal (spot+1) toInsert)
  in if x < 0 || x > 2 || y < 0 || y > 2 then Nothing
     else Just $ aux board y 0 (aux (board!!y) x 0 obj)


vBoardStatus :: Board Status -> Status
vBoardStatus [[a,b,c],[d,e,f],[g,h,i]] = 
  let possibleWins = [[a,b,c],
                      [d,e,f],
                      [g,h,i],
                      [a,d,g],
                      [b,e,h],
                      [c,f,i],
                      [a,e,i],
                      [c,e,g]]
  in if any (\x -> all (==(Over (Win P1))) x) possibleWins then Over $ Win P1 
     else if any (\x -> all (==(Over (Win P2))) x) possibleWins then Over $ Win P2
     else if not (any (\x ->any (==Ongoing) x) possibleWins) then Over $ Tie 
     else Ongoing


-- calculate if a player has won a board, helper of overall status
boardStatus :: Board Piece -> Status
boardStatus [[a,b,c],[d,e,f],[g,h,i]] = 
  let possibleWins = [[a,b,c],
                      [d,e,f],
                      [g,h,i],
                      [a,d,g],
                      [b,e,h],
                      [c,f,i],
                      [a,e,i],
                      [c,e,g]]
  in if any (\x -> all (==(Full P1)) x) possibleWins then Over $ Win P1 
     else if any (\x -> all (==(Full P2)) x) possibleWins then Over $ Win P2
     else if not (any (\x ->any (==Empty) x) possibleWins) then Over $ Tie 
     else Ongoing

assertTrue :: Bool -> Maybe ()
assertTrue False = Nothing
assertTrue True = Just ()
--make a move on master board
makeMove :: GameState -> Move -> Maybe GameState
makeMove (masterBoard, player, kMove) (boardLoc,pieceLoc) =
  do valid <- case kMove of Forced forcedLoc -> if forcedLoc == boardLoc then Just () else Nothing
                            Free             -> Just ()
     board <- boardIndex masterBoard boardLoc
     updatedBoard <- if boardIndex board pieceLoc == Just Empty
                     then boardInsert board pieceLoc (Full player)
                     else Nothing
     updatedMasterBoard <- boardInsert masterBoard boardLoc updatedBoard
     nextBoard <- boardIndex updatedMasterBoard pieceLoc
     let updatedPlayer = if player == P1 then P2 else P1
     let updatedKMove = if boardStatus nextBoard == Ongoing then (Forced pieceLoc) else Free
     return (updatedMasterBoard, updatedPlayer, updatedKMove)

boardEmptyLocations :: Board Piece -> [Loc]
boardEmptyLocations board = 
  let locPieces = zip [(x,y) | y <- [0..2], x <- [0..2]] (concat board)
      emptyLocs = filter (\(loc, piece) -> piece == Empty) locPieces
  in map (\(loc, piece) -> loc) emptyLocs

-- list of all valid moves given a gamestate
validMoves :: GameState -> [Move]
validMoves (masterBoard, player, Free) =
  let boards = zip [(x,y) | y <- [0..2], x <- [0..2]] (concat masterBoard)
      aux :: (Loc, (Board Piece)) -> [Move]
      aux (loc, board) = [ (loc, pieceLoc) | pieceLoc <- boardEmptyLocations board, boardStatus board== Ongoing]
  in concat (map aux boards)

validMoves (masterBoard, player, Forced forcedLoc) = 
  let maybeBoard = boardIndex masterBoard forcedLoc
  in case maybeBoard of 
    Nothing -> []
    Just board -> [ (forcedLoc, pieceLoc) | pieceLoc <- boardEmptyLocations board, boardStatus board == Ongoing]
  
opponent :: Player -> Player
opponent player = case player of P1 -> P2
                                 P2 -> P1

--will show the master board, splits the Master board by row, so that each row has the 3 games in
--that row. Then it reads whats in board using ShowBoard. Once that is complete it parses the Master
--Rows and puts them together for the output.
showGame :: GameState -> String
showGame (masterBoard, player ,kM) = 
            let rows = [row | row <- masterBoard]
                firstRow  = map showBoardHelper (head rows)
                secondRow = map showBoardHelper (head (tail rows))
                thirdRow  = map showBoardHelper (last rows)
                fillRow   = concat ((replicate 36 "=") ++ ["\n"])
                gameboard = "\n" ++ (parseRow firstRow) ++ fillRow ++(parseRow secondRow) ++ fillRow ++(parseRow thirdRow) ++ "\n"
                in gameboard

-- prints out ShowGame in a formated way
prettyShowGame :: GameState -> IO()
prettyShowGame (masterBoard, player ,kM) = let board = (showGame (masterBoard, player ,kM))
                      in do  putStrLn $ (prettyShowP player) ++ " is currently playing, they have a " ++ (prettyShowK kM) ++ "\n" ++ board

--take the masterBoard row, split it into subboard rows combine them together in order add filler
--rows to make viewing easier
parseRow :: [(String,String,String)] -> String
parseRow row = 
         let subRow1 = concat $ ([a | (a,b,c) <- row])
             subRow2 = concat $ ([b | (a,b,c) <- row])
             subRow3 = concat $ ([c | (a,b,c) <- row])
             fillrow = concat $ ["\n"] ++ (replicate 36 "-") ++ ["\n"]            
         in ((take 36 subRow1) ++ fillrow ++ (take 36 subRow2) ++fillrow ++ (take 36 subRow3)++ "\n") 

--take a board of Pieces split it into rows, then return the rows in order as a triple, is a helper,
--needs something to read its contents in a logcial fashion
showBoardHelper :: Board Piece -> (String,String,String)
showBoardHelper board = 
          let rows = [row | row <- board]
              one   = showRow [piece | piece <-(head rows)] 
              two   = showRow [piece | piece <-(head (tail rows))] 
              three = showRow [piece | piece <-(last rows)]
           in (one,two,three)

--take a row and format it nicely to show the pieces with splits shown
showRow :: Row Piece -> String
showRow (x:xs) = ((prettyShow x) ++ " | " ++ (prettyShow (head xs)) ++ " | " ++ (prettyShow (last xs)) ++ " || ")


prettyShow :: Piece -> String
prettyShow Empty = " "
prettyShow (Full P1)    = "X"
prettyShow (Full P2)    = "O"


prettyShowK :: KindOfMove -> String
prettyShowK Free = "allowed to play in any valid location"
prettyShowK (Forced (x,y))= "Forced to play in column " ++ (show (x+1)) ++ ", on row " ++ (show (y+1))

prettyShowP  :: Player -> String
prettyShowP P1 = "Player one"
prettyShowP P2 = "Player two"
