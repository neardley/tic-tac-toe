module Main where
import TicToe
import Solver
import HandleFiles
import Ranking
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import System.Exit
import Data.List.Split
import Debug.Trace


main :: IO ()
main = do args <- getArgs
          let (flags,inputs,errors) = getOpt Permute options args
          if (Help `elem` flags) || (not (null errors)) || (length inputs) > 1
          then putStrLn $ usageInfo "Usage: TicTacToe [options] [file]" options
          else 
                let depth = getDepth flags  -- depth flag handled here
               in if null inputs 
                 then if Interactive `elem` flags
                      then playGame firstGameState depth P1
                      else error "need a file!" 
                 else do let filename = head inputs -- filename input
                         maybefile <- fileToGameState filename
                         case maybefile of
                           Nothing -> putStrLn "The file was not properly created" 
                           Just gs@(bs,player,kmove) -> if(Winner `elem` flags)     -- winner flag here
                                                         then handleWinner gs
                                                         else if(getMove flags /= ((4,4),(4,4))) -- move flag here
                                                               then handleMove gs flags 
                                                               else if Interactive `elem` flags   -- interactive flag
                                                                     then playGame gs depth player
                                                                     else case  goodMove gs depth of -- default begins
                                                                           Nothing -> putStrLn "game is over, you cannot win what is won!"
                                                                           Just godMove -> if (Verbose `elem` flags)  -- verbose 
                                                                                            then putStrLn $ "make move " ++ (niceShowMove godMove) ++ " move ranked as " ++(parseScore (rankMove gs godMove))
                                                                                            else putStrLn $ (niceShowMove godMove) -- default output
        





data Flag =  Help | Winner | Depth String | MakeMove String | Verbose | Interactive deriving (Eq,Show) 
 

options :: [OptDescr Flag]
options = [
  Option ['h'] ["help"]         (NoArg   Help)                  "Print a help message and exit.",
  Option ['w'] ["winner"]       (NoArg   Winner)                "Go to the ends of the earth to tell me who can win this may take eternity",  
  Option ['d'] ["depth"]        (ReqArg  Depth  "<num>")            "have max depth of best moves be inputed",
  Option ['m'] ["move"]         (ReqArg   MakeMove  "<w,x,y,z>") "Make move where first pair is location on masterboard and second is location on the subboard",
  Option ['v'] ["verbose"]      (NoArg  (Verbose ))              "Display more information during processing." , 
  Option ['i'] ["interactive"]  (NoArg (Interactive) )           "Play the game aganst the computer"
  ]






getDepth :: [Flag] -> Int
getDepth ((Depth s):_) = read s
getDepth (_:flags) = getDepth flags
getDepth [] = 3


getMove :: [Flag] -> (Loc,Loc)
getMove ((MakeMove s):_) = stringToLocs s
getMove (_:flags) = getMove flags
getMove [] = ((4,4),(4,4))



stringToLocs :: String -> (Loc,Loc)
stringToLocs mv =
    let break = splitOn "," mv
        mstep1 = take 2 break
        sstep1 = drop 2 break
        mLoc = (read (head mstep1)-1, read (last mstep1)-1)
        sLoc = (read (head sstep1)-1, read (last sstep1)-1)
        in (mLoc,sLoc)

handleWinner :: GameState -> IO ()
handleWinner game = do
    let maybebstMove = bestMove game
    case maybebstMove of
         (Nothing) -> putStrLn $ "A best mve cannont be found"
         Just bstMove  -> putStrLn  $ "the best move is " ++ (show bstMove)


handleMove :: GameState -> [Flag] -> IO()
handleMove game flags = do
    let (mLoc,sLoc) = getMove flags
    let maybeMoved = makeMove game (mLoc,sLoc)
    case maybeMoved of 
        Nothing -> putStrLn "improper move made"
        Just moved  -> putStrLn $ stateToString moved


verifyGameState :: Maybe GameState -> GameState
verifyGameState Nothing = error "Improper file"
verifyGameState (Just gs) = gs

moveToString :: Maybe Move -> String
moveToString Nothing = "Game is already Over, can't win what is won."
moveToString (Just move) = "make the move " ++ (show move) ++ " for the best outcome."

rankMove :: GameState -> Move -> Int
rankMove (mboard,player,kMove) godMove = 
    let before = rank (mboard,player,kMove)
        maybeAfter = makeMove (mboard,player,kMove) godMove
    in case maybeAfter of
           Nothing -> before
           Just (gs,player,kmove) -> let afterRank = rank (gs,P1,kMove)
                                     in if afterRank == 1000 || afterRank == -1000 || afterRank == 0
                                        then afterRank
                                        else (afterRank - before)


parseScore :: Int -> String
parseScore score = case score of
        1000  -> "Win for Player 1!"
        -1000 -> "Win for Player 2!"
        0     -> "a Tie, Perfectly Balanced as all things should be"
        otherwise    -> show score


niceShowMove :: Move -> String
niceShowMove ((w,x),(y,z)) = 
        let move = ((w+1,x+1),(y+1,z+1))
        in show move

playGame :: GameState -> Int -> Player -> IO()
playGame (mboard,player,kMove) depth human = 
          let status = overallStatus (mboard,player,kMove)
          in case status of
             Over (Win P1) -> if P1 == human 
                              then putStrLn "You have won the game!!!" 
                              else putStrLn "computer as won, you have lost"
             Over (Win P2) -> if P2 == human 
                              then putStrLn "You have won the game!!!" 
                              else putStrLn "computer as won, you have lost"
             Over Tie      -> putStrLn "a Tie, Perfectly Balanced as all things should be"     
             Ongoing       -> if player == human
                              then playerTurn (mboard,player,kMove) depth human
                              else computerTurn (mboard,player,kMove) depth human 

playerTurn :: GameState -> Int -> Player -> IO()
playerTurn (mboard,player,kMove) depth human =
    do 
       putStrLn $ "It is your turn you are  " ++ prettyShowK kMove ++ " here is the board, please enter your move as: game,row,x,y"
       putStrLn $ showGame (mboard,player,kMove)
       maybeMoveString <- getLine
       let maybeMove = (stringToLocs maybeMoveString)
       end <- case makeMove (mboard,player,kMove) maybeMove of
                     Nothing -> do putStrLn "Invalid move made, please try again... " 
                                   (playerTurn (mboard,player,kMove) depth human)
                     Just newState -> do putStrLn $ showGame newState
                                         playGame newState depth human
       return ()

computerTurn ::  GameState -> Int -> Player -> IO()
computerTurn (mboard,player,kMove) depth human =
    do putStrLn "Computer's Turn"
       let maybeMove = goodMove (mboard,player,kMove) depth
       let move = case maybeMove of
                       Nothing -> ((4,4),(4,4))
                       Just mv -> mv
       newState <- case (makeMove (mboard,player,kMove) move) of
                       Nothing -> do putStrLn "Game is over, I hope..."
                       Just gs -> do putStrLn ("Computer has moved to " ++ (niceShowMove move)) 
                                     playGame gs depth human
       return ()                             
    
        



