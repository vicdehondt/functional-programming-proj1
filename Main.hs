{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (isAlphaNum)
import Control.Monad (unless)
import Text.Read (readMaybe)
import System.Random (mkStdGen, randomR)

-- user input for a simple terminal-based game is just a single-line string
type Command = String

promptForInput :: IO Command
promptForInput = putStr "> " >> fmap (filter isAlphaNum) getLine

-- We use a type s to represent a game state, where ...
-- ... nextState computes the next game state, given the current state and next user input (may fail on invalid input)
-- ... isFinalState checks whether a given state is a final state 
class GameState s where
    nextState :: s -> Command -> Maybe s
    isFinalState :: s -> Bool

-- To "boot" a terminal-based game, we use a type s to represent game state and a type c to represent game configuration, where ...
-- ... we can compute an initial game state s using a given configuration c (which can fail if the configuration is invalid)
class GameState s => TerminalGame s c | c -> s where
    initialState :: c -> Either String s

-- run a game in the terminal
runGame :: (Show s, TerminalGame s c) => c -> IO ()
runGame = either error loop . initialState
    where loop st = do print st
                       unless (isFinalState st) $ do
                            cmd <- promptForInput
                            let nxt = nextState st cmd
                            maybe (putStrLn "Invalid input, please try again" >> loop st) loop nxt

--
-- EXAMPLE: Guess the number! 
--

data GuessingGameConfig = GuessingGameConfig { seed :: Int, low :: Int, high :: Int }
data GuessingGameState = GuessingGameState { target :: Int, lastGuess :: Guess } 
data Guess = NoGuess | Guess Int 

instance GameState GuessingGameState where
    nextState GuessingGameState{..} input = fmap (GuessingGameState target . Guess) (readMaybe input) --readMaybe returns Nothing if the input string can't be parsed as an integer
    isFinalState GuessingGameState{lastGuess = Guess last, ..} = last == target
    isFinalState _ = False 

instance TerminalGame GuessingGameState GuessingGameConfig where
    initialState GuessingGameConfig{..}
        | low < high = let (num, _) = randomR (low, high) (mkStdGen seed) in Right (GuessingGameState num NoGuess)
        | otherwise = Left "Invalid configuration: low should be smaller than high"

instance Show GuessingGameState where
    show GuessingGameState{..} = report lastGuess
        where report NoGuess = "Make a guess"
              report (Guess last)
                | target < last = "Aim lower!"
                | target > last = "Aim higher!"
                | otherwise = "Congratulations!" -- target == last, you won!

main = runGame GuessingGameConfig { seed = 42, low = 0, high = 100 }