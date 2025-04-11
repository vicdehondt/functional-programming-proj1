{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (isAlphaNum)
import Control.Monad (unless)
import Text.Read (readMaybe)
import System.Random (mkStdGen, randomR, splitGen, random, RandomGen, Random, SplitGen)

-- user input for a simple terminal-based game is just a single-line string
type Command = String

-- Percentage type for game configuration
newtype Percentage = Percentage Double
    deriving (Show, Eq, Ord)

makePercentage :: Double -> Maybe Percentage
makePercentage percentage
    | percentage >= 0 && percentage <= 100 = Just (Percentage percentage)
    | otherwise = Nothing

readWASD :: [Char] -> Maybe Char
readWASD string
    | length string == 1 && head string `elem` "wasd" = Just $ head string
    | otherwise = Nothing

data Tile = Desert | Water | Lava | Portal
    deriving (Show, Enum, Bounded, Eq)

instance Random Tile where
    randomR (minTile, maxTile) gen =
        let (n, gen') = randomR (fromEnum minTile, fromEnum maxTile) gen
        in (toEnum n, gen')

    random = randomR (minBound, maxBound) -- random gen = random because randomR takes gen as well.

randomTiles :: RandomGen g => g -> [Tile]
randomTiles gen = tile : randomTiles gen'
  where
    (tile, gen') = random gen

randomTilesList :: SplitGen g => g -> [[Tile]]
randomTilesList gen = tiles : randomTilesList gen'
    where
        (gen', gen'') = splitGen gen
        tiles = randomTiles gen''

generateTilesList :: Int -> [[Tile]]
generateTilesList seed = randomTilesList (mkStdGen seed)

getTileAt :: Int -> Int -> [[Tile]] -> Tile
getTileAt x y tileLists = (tileLists !! y) !! x

startIndex :: Int -> Int -> Int
startIndex x s =
    if odd s then
        x - (s `div` 2)
    else
        x - (s `div` 2) + 1

endIndex :: Int -> Int -> Int
endIndex x s = x + (s `div` 2)

startAndEnd :: Int -> Int -> (Int, Int)
startAndEnd start end =
    if start < 0 then
        (0, end - start)
    else
        (start, end)

-- Herschrijven???
showGrid :: Int -> Int -> Int -> [[Tile]] -> [Char]
showGrid playerX playerY s tileLists =
    let (firstX, lastX) = startAndEnd (startIndex playerX s) (endIndex playerX s)
        (firstY, lastY) = startAndEnd (startIndex playerY s) (endIndex playerY s)
        rows = [[showTile x' y' playerX playerY tileLists | x' <- [firstX..lastX]] | y' <- [firstY..lastY]]
    in unlines rows

-- Herschrijven???
showTile :: Int -> Int -> Int -> Int -> [[Tile]] -> Char
showTile x y playerX playerY tileLists
    | x == playerX && y == playerY = '@'
    | otherwise = case getTileAt x y tileLists of
        Desert -> 'D'
        Water  -> 'W'
        Lava   -> 'L'
        Portal -> 'P'

inputToLocation :: Location -> [[Tile]] -> Char -> Location
inputToLocation (Location x y tile) map input
    | input == 'w' && y > 0 = let newY = y - 1 in Location x newY (getTileAt x newY map)
    | input == 'a' && x > 0 = let newX = x - 1 in Location newX y (getTileAt newX y map)
    | input == 's' = let newY = y + 1 in Location x newY (getTileAt x newY map)
    | input == 'd' = let newX = x + 1 in Location newX y (getTileAt newX y map)
    | otherwise = Location x y tile


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


-- GAME

data DesertExplorerGameConfig = DesertExplorerGameConfig { s :: Int, m :: Int, g :: Int, t :: Percentage, w :: Percentage, p :: Percentage, l :: Percentage, ll :: Percentage}
data DesertExplorerGameState = DesertExplorerGameState { map :: [[Tile]], lineOfSight :: Int, lastLocation :: Location }
data Location = Location { x :: Int, y :: Int, tile :: Tile }

instance GameState DesertExplorerGameState where
    nextState DesertExplorerGameState{..} input = fmap (DesertExplorerGameState map lineOfSight . inputToLocation lastLocation map) (readWASD input) --readWASD returns Nothing if the input string can't be parsed as a WASD character
    isFinalState DesertExplorerGameState{lastLocation = Location x y tile, ..} = tile == Portal || tile == Lava
    isFinalState _ = False

instance TerminalGame DesertExplorerGameState DesertExplorerGameConfig where
    initialState DesertExplorerGameConfig{..}
        | s > 0 && m > 0 = let gameMap = generateTilesList g in Right (DesertExplorerGameState gameMap s (Location 0 0 (getTileAt 0 0 gameMap)))
        | otherwise = Left "Invalid configuration"

instance Show DesertExplorerGameState where
    show DesertExplorerGameState{ lastLocation = Location playerX playerY tile, ..} =
        showGrid playerX playerY lineOfSight map ++ "\n" ++ report tile
        where report tile
                | tile == Lava = "You died, GAME OVER"
                | tile == Portal = "Congratulations!" -- Portal tile, you won!
                | otherwise = "Move!"

main = runGame DesertExplorerGameConfig { s = 3, m = 10, g = 46, t = Percentage 50, w = Percentage 20, p = Percentage 10, l = Percentage 5, ll = Percentage 5 }






--
-- EXAMPLE: Guess the number! 
--

-- data GuessingGameConfig = GuessingGameConfig { seed :: Int, low :: Int, high :: Int }
-- data GuessingGameState = GuessingGameState { target :: Int, lastGuess :: Guess }
-- data Guess = NoGuess | Guess Int

-- instance GameState GuessingGameState where
--     nextState GuessingGameState{..} input = fmap (GuessingGameState target . Guess) (readMaybe input) --readMaybe returns Nothing if the input string can't be parsed as an integer
--     isFinalState GuessingGameState{lastGuess = Guess last, ..} = last == target
--     isFinalState _ = False

-- instance TerminalGame GuessingGameState GuessingGameConfig where
--     initialState GuessingGameConfig{..}
--         | low < high = let (num, _) = randomR (low, high) (mkStdGen seed) in Right (GuessingGameState num NoGuess)
--         | otherwise = Left "Invalid configuration: low should be smaller than high"

-- instance Show GuessingGameState where
--     show GuessingGameState{..} = report lastGuess
--         where report NoGuess = "Make a guess"
--               report (Guess last)
--                 | target < last = "Aim lower!"
--                 | target > last = "Aim higher!"
--                 | otherwise = "Congratulations!" -- target == last, you won!

-- main = runGame GuessingGameConfig { seed = 42, low = 0, high = 100 }