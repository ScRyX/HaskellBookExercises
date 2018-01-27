module Morra where

import Data.Monoid
import System.Random
import Control.Monad.Trans.State
import Control.Monad.Trans

data GameState = 
  GameState {
    playerPoints :: Int
  , aiPoints :: Int
  , genState :: StdGen
  } deriving Show

defaultState :: IO GameState
defaultState = do
  g <- newStdGen 
  return $ GameState { playerPoints = 0, aiPoints = 0, genState = g }

playerScores :: GameState -> GameState
playerScores gs = gs { playerPoints = playerPoints gs + 1 }

aiScores :: GameState -> GameState
aiScores gs = gs { aiPoints = aiPoints gs + 1 }

updateScore :: GameState -> Int -> Int -> GameState
updateScore gs g1 g2 = 
  case (even (g1 + g2)) of
    True      -> playerScores gs
    otherwise -> aiScores gs

aiTurn :: StdGen -> (Int, StdGen)
aiTurn g = randomR (1,10) g

isGameOver :: GameState -> Bool
isGameOver gs = (aiPoints gs == 5) || (playerPoints gs == 5)

announceWinner :: GameState -> String
announceWinner gs = do
  if aiPoints gs > 4 then "Computer wins!"
  else "Player wins!"

getScoreMsg :: GameState -> String
getScoreMsg gs = mconcat ["PC | ",(show $ aiPoints gs),":",(show $ playerPoints gs)," | Player"]

playGame :: StateT GameState IO String 
playGame = do
  gs <- get
  if isGameOver gs then return $ announceWinner gs
  else do
    x <- lift getLine
    let g = read x
    let (aiGuess, newGen) = aiTurn $ genState gs
    let newGs = (updateScore gs g aiGuess) { genState = newGen } 
    put newGs
    let msg = (show $ aiPoints newGs) ++ ":" ++ (show $ playerPoints newGs)
    lift $ putStrLn . getScoreMsg $ newGs 
    playGame 

test :: StateT GameState IO String 
test = do
  gs <- lift defaultState 
  str <- lift $ runStateT (playGame) $ gs
  return (fst str)

main :: IO () 
main = do
  ds <- defaultState 
  (msg,_) <- (runStateT test) ds
  print msg

