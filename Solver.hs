module Solver where
import TicToe
import Data.Maybe

bestForPlayer :: Player -> [Outcome] -> Outcome
bestForPlayer player outcomeLst =
    if (Win player) `elem` outcomeLst
    then (Win player)
    else if (Tie) `elem` outcomeLst
         then (Tie)
         else Win (opponent player)

outcome :: GameState -> Outcome
outcome gs@(mb, player, kMove) = 
    let status = overallStatus gs
        moves = validMoves gs
        gStates = catMaybes $ map (makeMove gs) moves
        outcomeLst = map outcome gStates
    in case status of Over out -> out
                      Ongoing -> bestForPlayer player outcomeLst


seqTuple :: (Maybe a, b) -> Maybe (a,b)
seqTuple (Nothing, _) = Nothing
seqTuple (Just x, y) = Just (x,y)

bestMove :: GameState -> Maybe Move
bestMove gs@(mb, player, kMove) = 
    let status = overallStatus gs
        moves = validMoves gs    
        maybeGS = [(res,move) | move <- moves, let res = makeMove gs move]
        gameStates = catMaybes $ map seqTuple maybeGS
        results = map (\(g,m) -> (outcome g,m)) gameStates
        outcomes = map (\(o,m) -> o) results
        best = bestForPlayer player outcomes
    in case status of Over out -> Nothing 
                      Ongoing -> lookup best results
