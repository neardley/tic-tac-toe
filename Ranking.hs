module Ranking where
import TicToe
import Data.Maybe
import Debug.Trace

count :: Eq a => [a] -> a -> Int
count [] n = 0
count (x:xs) n = if x==n then 1 + (count xs n) else count xs n

out :: Player -> [Piece] -> Int
out player ps = 
  let playerCount = count ps (Full player)
      opponentCount = count ps (Full (opponent player))
  in if opponentCount > 0  then 0
     else case playerCount of 0 -> 1
                              1 -> 2
                              2 -> 4
                              3 -> 40


boardRank :: SubBoard -> Int
boardRank [[a,b,c],[d,e,f],[g,h,i]] = 
  let possibleWins = [[a,b,c],
                      [d,e,f],
                      [g,h,i],
                      [a,d,g],
                      [b,e,h],
                      [c,f,i],
                      [a,e,i],
                      [c,e,g]]
      p1PointLst = map (out P1) possibleWins
      p2PointLst = map (out P2) possibleWins
  in if 40 `elem` p1PointLst then 40
     else if 40 `elem` p2PointLst then -40
     else (sum p1PointLst - sum p2PointLst)



rank :: GameState -> Int
rank gs@(mb, player, kMove) =
  let status = overallStatus gs
      playerValue = case player of P1 -> 1
                                   P2 -> -1
      movePoints = case kMove of Free -> 9 * playerValue
                                 otherwise -> 1 * playerValue
      boardTotals = map boardRank (concat mb)
      mboardValue = sum $ zipWith (*) boardTotals [3,2,3,2,4,2,3,2,3]
  in case status of Over (Win P1) -> 1000
                    Over (Win P2) -> -1000
                    Over Tie -> 0
                    otherwise -> movePoints + mboardValue

bestForPlayer :: Player -> [Int] -> Int
bestForPlayer P1 ranks = maximum ranks
bestForPlayer P2 ranks = minimum ranks

--rankTheGameState
rankGameState:: Int -> GameState -> Int
rankGameState 0 gs = rank gs
rankGameState n gs@(mb, player, kMove) = 
    let status = overallStatus gs
        moves = validMoves gs
        gStates = catMaybes $ map (makeMove gs) moves
        rankLst = map (rankGameState (n-1)) gStates
        best =  bestForPlayer player rankLst
    in  case status of 
          Over (Win P1) -> 1000
          Over (Win P2) -> -1000
          Over Tie -> 0
          otherwise -> best

seqTuple :: (Maybe a, b) -> Maybe (a,b)
seqTuple (Nothing, _) = Nothing
seqTuple (Just x, y) = Just (x,y)

goodMove :: GameState -> Int -> Maybe Move
--goodMove gs 0 = 

goodMove gs@(mb, player, kMove) n = 
    let startingRank = rank gs
        status = overallStatus gs
        moves = validMoves gs 
        maybeGS = [(res,move) | move <- moves, let res = makeMove gs move]
        gameStates = catMaybes $ map seqTuple maybeGS
        results = map (\(g,m) -> (rankGameState (n-1) g,m)) gameStates
        rankLst = map (\(o,m) -> o) results
        best = bestForPlayer player rankLst
    in case status of Over _ -> Nothing
                      otherwise -> lookup best results
