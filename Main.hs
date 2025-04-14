{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}

import Data.Char (isAlphaNum)
import Control.Monad (unless)
import Text.Read (readMaybe)
import System.Random (mkStdGen, randomR, splitGen, random, RandomGen, Random, SplitGen)

import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import GHC.Exts.Heap (GenClosure(value))

-- user input for a simple terminal-based game is just a single-line string
type Command = String

-- Percentage type for game configuration
newtype Percentage = Percentage Int
    deriving (Show, Eq, Ord)

-- Keep water supply and maximum water supply together
data WaterSupply = WaterSupply { current :: Int, maximum :: Int }

-- Ensure percentages are between 0 and 100
makePercentage :: Int -> Percentage
makePercentage percentage
    | percentage >= 0 && percentage <= 100 = Percentage percentage
    | otherwise = error "Percentage value out of bounds"

-- Get the exact value of a percentage in Int form
fromPercentage :: Percentage -> Int
fromPercentage (Percentage p) = p

-- Tile type for the map/grid of the game
-- Desert Tiles keep a boolean to know whether there is a treasure or not
data Tile = Desert Bool | Water | Lava | Portal
    deriving (Show, Eq)

-- Because of "Desert Bool", Enum needs to be explicitly defined
instance Enum Tile where
    toEnum 0 = Desert False
    toEnum 1 = Water
    toEnum 2 = Lava
    toEnum 3 = Portal

    fromEnum (Desert _) = 0
    fromEnum Water      = 1
    fromEnum Lava       = 2
    fromEnum Portal     = 3

-- Because of "Desert Bool", Bounded needs to be explicitly defined
instance Bounded Tile where
    minBound = Desert False
    maxBound = Portal

-- Simple location type
data Location = Location { x :: Int, y :: Int, tile :: Tile }

-- For the Ord instance
instance Eq Location where
  (Location x1 y1 tile1) == (Location x2 y2 tile2) = (x1, y1) == (x2, y2)

-- To add to a Set for example 
instance Ord Location where
  compare (Location x1 y1 _) (Location x2 y2 _) = compare (x1, y1) (x2, y2)

-- Keep a data type for the chances defined in the GameConfig
data TileChances = TileChances
  { treasureChance :: Percentage
  , waterChance :: Percentage
  , portalChance :: Percentage
  , singleLavaChance :: Percentage
  , adjacentLavaChance :: Percentage
  }

-- Check whether there is a lava tile directly above or left of
-- the current position in the map/grid
lavaAdjacent :: Maybe Tile -> [Tile] -> Int -> Bool
lavaAdjacent tileLeft tilesAbove x =
  tileLeft == Just Lava || safeIndex tilesAbove x == Just Lava

-- To index in a list without evaluating everything
-- Necessary for infinite lists (now no length check that would eval every element)
-- Found drop on Hoogle: https://hoogle.haskell.org/?hoogle=drop&scope=set%3Astackage
safeIndex :: [Tile] -> Int -> Maybe Tile
safeIndex tileList index
    | index < 0 = Nothing
    | otherwise = case drop index tileList of
        (tile:_) -> Just tile
        []    -> Nothing

-- Negated lavaAdjecent
noLavaAdjacent :: Maybe Tile -> [Tile] -> Int -> Bool
noLavaAdjacent left above x = not (lavaAdjacent left above x)

-- Generate a random Tile based in the chances defined in the Game Config
generateTile :: RandomGen g => TileChances -> g -> Int -> [Tile] -> Maybe Tile -> (Tile, g)
generateTile TileChances{..} gen x tilesAbove tileLeft=
  let (r, gen') = randomR (1, 100 :: Int) gen
      water = fromPercentage waterChance
      portal = fromPercentage portalChance
      singleLava = fromPercentage singleLavaChance
      adjacentLava = fromPercentage adjacentLavaChance
  in case () of
       _ | r <= water  -> (Water, gen')
         | r <= water + portal -> (Portal, gen')
         | noLavaAdjacent tileLeft tilesAbove x && (r <= water + portal + singleLava) -> (Lava, gen')
         | lavaAdjacent tileLeft tilesAbove x && (r <= water + portal + adjacentLava) -> (Lava, gen')
         | otherwise -> let (randomTreasureInt, gen'') = randomR (1, 100 :: Int) gen
                            treasure = fromPercentage treasureChance in
                                if randomTreasureInt <= treasure then
                                    (Desert True, gen'')
                                else
                                    (Desert False, gen'')

-- Hebben we name binding gezien? "chances@" lijkt nieuw voor mij...
-- Generate an infinite list of random Tiles based on the chances defined in the Game Config
randomTiles :: RandomGen g => TileChances -> g -> Int -> [Tile] -> Maybe Tile -> [Tile]
randomTiles chances@TileChances{..} gen currentX previousYTiles lastTile =
    let (tile, gen') = generateTile chances gen currentX previousYTiles lastTile in
        tile : randomTiles chances gen' (currentX + 1) previousYTiles (Just tile)

-- Generate an infinite list of infinite lists of random Tiles
-- Each sublist is a level on the y-axis of the map/grid
-- Two generators are needed to have each sublist different from the sublist before
-- Found splitGen on Hoogle: https://hoogle.haskell.org/?hoogle=splitGen
randomTilesList :: SplitGen g => TileChances -> g -> [Tile] -> [[Tile]]
randomTilesList chances gen previousTiles =
    let (gen1, gen2) = splitGen gen
        tiles = randomTiles chances gen1 0 previousTiles Nothing
    in tiles : randomTilesList chances gen2 tiles

-- Generate an infinite list of infinite lists of Tiles based off a given seed number
generateTilesList :: Int -> TileChances -> [[Tile]]
generateTilesList seed chances = randomTilesList chances (mkStdGen seed) []

-- Get a Tile based on its x and y position in the map/grid
getTileAt :: Int -> Int -> [[Tile]] -> Tile
getTileAt x y tileLists = (tileLists !! y) !! x

-- Get the start index of tiles to display based on the players line of sight
startIndex :: Int -> Int -> Int
startIndex x s =
    if odd s then
        x - (s `div` 2)
    else
        x - (s `div` 2) + 1

-- Get the end index of tiles to display based on the players line of sight
endIndex :: Int -> Int -> Int
endIndex x s = x + (s `div` 2)

-- Combine startIndex and endIndex to get the indices of tiles to display
-- The player is always in the center of the map/grid unless the indices would become negative
startAndEnd :: Int -> Int -> (Int, Int)
startAndEnd start end =
    if start < 0 then
        (0, end - start)
    else
        (start, end)

-- Get the displayable map/grid based on a players position (x and y)
-- Found unlines on Hoogle: https://hoogle.haskell.org/?hoogle=unlines
getGrid :: Int -> Int -> Int -> [[Tile]] -> [Char]
getGrid playerX playerY s tileLists =
    let (firstX, lastX) = startAndEnd (startIndex playerX s) (endIndex playerX s)
        (firstY, lastY) = startAndEnd (startIndex playerY s) (endIndex playerY s)
        rows = [[showTile x y playerX playerY tileLists | x <- [firstX..lastX]] | y <- [firstY..lastY]]
    in unlines rows

-- Convert a tile into a Char representation
-- When the player is standing on the tile, give it the character '@'
showTile :: Int -> Int -> Int -> Int -> [[Tile]] -> Char
showTile x y playerX playerY tileLists
    | x == playerX && y == playerY = '@'
    | otherwise = convert (getTileAt x y tileLists) where
        convert (Desert _) = 'D'
        convert Water = 'W'
        convert Lava = 'L'
        convert Portal = 'P'

-- Get an exact character value of a Maybe Int or an infinite character when Nothing
extractValue :: Maybe Int -> [Char]
extractValue (Just value) = show value
extractValue Nothing = "∞"

-- Compute the closest water Tile
getWaterDistance :: Int -> Int -> [[Tile]] -> [Char]
getWaterDistance playerX playerY tileLists = extractValue (closestTileLazy (== Water) tileLists (Location playerX playerY (getTileAt playerX playerY tileLists)))

-- Compute the closest desert Tile
getDesertDistance :: Int -> Int -> [[Tile]] -> [Char]
getDesertDistance playerX playerY tileLists = extractValue (closestTileLazy isDesert tileLists (Location playerX playerY (getTileAt playerX playerY tileLists)))
    where
        isDesert (Desert _) = True
        isDesert _ = False

-- Compute the closest portal Tile
getPortalDistance :: Int -> Int -> [[Tile]] -> [Char]
getPortalDistance playerX playerY tileLists = extractValue (closestTileLazy (== Portal) tileLists (Location playerX playerY (getTileAt playerX playerY tileLists)))

-- Standard breadth-first search algorithm to get the closest tile
-- a queue to keep the path and a set to track visited locations
closestTileLazy :: (Tile -> Bool) -> [[Tile]] -> Location -> Maybe Int
closestTileLazy match tileList startLocation = go (Seq.singleton (startLocation, 0)) Set.empty
  where
    go Seq.Empty _ = Nothing  -- Shouldn't happen in an infinite world unless no water
    go ((location@(Location x y tile), dist) Seq.:<| queue) visited
      | Set.member location visited = go queue visited -- Already visited, skip
      | tile == Lava = go queue (Set.insert location visited) -- Lava, add to visited
      | dist > 0 && match tile = Just dist -- Not current position and is tile we are looking for, found
      | otherwise = -- Keep searching
          let neighbors =
                filter (\(Location x y _) -> x >= 0 && y >= 0) -- Get all directions but don't go negative
                    [ Location (x + 1) y (getTileAt (x + 1) y tileList),
                      Location (x - 1) y (getTileAt (x - 1) y tileList),
                      Location x (y + 1) (getTileAt x (y + 1) tileList),
                      Location x (y - 1) (getTileAt x (y - 1) tileList)]
              queue' = queue Seq.>< Seq.fromList [(newLocation, dist+1) | newLocation <- neighbors] -- Add new locations to end of queue (bfs)
              visited' = Set.insert location visited -- Current position is visited
          in go queue' visited'

-- Subtract 1 unit of the water supply (when the player moved)
-- or refill the supply (moved to a water Tile)
updateWaterSupply :: Location -> WaterSupply -> WaterSupply
updateWaterSupply (Location x y tile) (WaterSupply current maximum)
    | tile == Water = WaterSupply maximum maximum
    | otherwise = WaterSupply (current - 1) maximum

-- Add treasure points when a player is on a desert Tile with a treasure on it
updateTreasureWorth :: Location -> Int -> Int
updateTreasureWorth (Location x y tile) oldTreasureWorth =
    if tile == Desert True then
        oldTreasureWorth + 10
    else
        oldTreasureWorth

-- Same as isAlphaNum but for WASD keys
readWASD :: [Char] -> Maybe Char
readWASD string
    | length string == 1 && head string `elem` "wasd" = Just $ head string
    | otherwise = Nothing

-- GIVEN BY NOAH
promptForInput :: IO Command
promptForInput = putStr "> " >> fmap (filter isAlphaNum) getLine

-- We use a type s to represent a game state, where ...
-- ... nextState computes the next game state, given the current state and next user input (may fail on invalid input)
-- ... isFinalState checks whether a given state is a final state
-- GIVEN BY NOAH
class GameState s where
    nextState :: s -> Command -> Maybe s
    isFinalState :: s -> Bool

-- To "boot" a terminal-based game, we use a type s to represent game state and a type c to represent game configuration, where ...
-- ... we can compute an initial game state s using a given configuration c (which can fail if the configuration is invalid)
-- GIVEN BY NOAH
class GameState s => TerminalGame s c | c -> s where
    initialState :: c -> Either String s

-- run a game in the terminal
-- GIVEN BY NOAH
runGame :: (Show s, TerminalGame s c) => c -> IO ()
runGame = either error loop . initialState
    where loop st = do print st
                       unless (isFinalState st) $ do
                            cmd <- promptForInput
                            let nxt = nextState st cmd
                            maybe (putStrLn "Invalid input, please try again" >> loop st) loop nxt

-- Check that the given game parameters are playable
validGameParameters :: DesertExplorerGameConfig -> Bool
validGameParameters DesertExplorerGameConfig {..} =
    let waterChance = fromPercentage w
        portalChance = fromPercentage p
        lavaChance = fromPercentage l
        lavaLavaChance = fromPercentage ll in
            s > 0 && m > 0 && waterChance + portalChance + lavaChance <= 100 && waterChance + portalChance + lavaLavaChance <= 100

-- Get the location based on the given input
-- When the input is incorrect (not WASD), return Nothing
inputToLocation :: Location -> [[Tile]] -> Maybe Char -> Maybe Location
inputToLocation (Location x y tile) map input
    | input == Just 'w' && y > 0 = let newY = y - 1 in Just (Location x newY (getTileAt x newY map))
    | input == Just 'a' && x > 0 = let newX = x - 1 in Just (Location newX y (getTileAt newX y map))
    | input == Just 's' = let newY = y + 1 in Just (Location x newY (getTileAt x newY map))
    | input == Just 'd' = let newX = x + 1 in Just (Location newX y (getTileAt newX y map))
    | otherwise = Nothing



-- GAME

-- Parameters as mentioned in the assignment
data DesertExplorerGameConfig = DesertExplorerGameConfig { s :: Int, m :: Int, g :: Int, t :: Percentage, w :: Percentage, p :: Percentage, l :: Percentage, ll :: Percentage}
-- The map/grid, line of sight, treasure points, water supply and location is kept in state
data DesertExplorerGameState = DesertExplorerGameState { map :: [[Tile]], lineOfSight :: Int, treasureWorth :: Int, waterSupply :: WaterSupply, lastLocation :: Location }

-- Update the state every input to update the water supply, treasure points and location of the player
-- Using fmap to map the location to the game state: https://hoogle.haskell.org/?hoogle=fmap
-- The game is in a final state when the player is on a portal Tile, a lava Tile or when the water supply is empty
-- GIVEN BY NOAH, adapted for assignment
instance GameState DesertExplorerGameState where
    nextState DesertExplorerGameState{..} input =
        fmap (\newLocation -> DesertExplorerGameState
        { map = map
        , lineOfSight = lineOfSight
        , treasureWorth = updateTreasureWorth newLocation treasureWorth
        , waterSupply = updateWaterSupply newLocation waterSupply
        , lastLocation = newLocation
        }) (inputToLocation lastLocation map (readWASD input)) --readWASD returns Nothing if the input string can't be parsed as a WASD character
    isFinalState DesertExplorerGameState{lastLocation = Location x y tile, waterSupply = WaterSupply current _, ..} = tile == Portal || tile == Lava || current == 0

-- Instantiate the tile chances and start the game at location (0, 0) with a full water supply and no treasure points
-- GIVEN BY NOAH, adapted for assignment
instance TerminalGame DesertExplorerGameState DesertExplorerGameConfig where
    initialState DesertExplorerGameConfig{..}
        | validGameParameters DesertExplorerGameConfig{..} =
            let gameMap = generateTilesList g TileChances { treasureChance = t, waterChance = w, portalChance = p, singleLavaChance = l, adjacentLavaChance = ll } in
                Right (DesertExplorerGameState gameMap s 0 (WaterSupply m m) (Location 0 0 (getTileAt 0 0 gameMap)))
        | otherwise = Left "Invalid configuration"

-- Display the map/grid and state of the game
-- Display the necessary message and information when won or lost
-- GIVEN BY NOAH, adapted for assignment
instance Show DesertExplorerGameState where
    show DesertExplorerGameState{ lastLocation = Location playerX playerY tile, waterSupply = WaterSupply current maximum, .. } =
        unlines [
            [],
            getGrid playerX playerY lineOfSight map,
            "Total treasure worth: " ++ show treasureWorth,
            "Total water left: " ++ show current ++ "/" ++ show maximum,
            "Closest water tile: " ++ getWaterDistance playerX playerY map,
            "Closest desert tile: " ++ getDesertDistance playerX playerY map,
            "Closest portal tile: " ++ getPortalDistance playerX playerY map,
            [],
            report waterSupply tile
        ]
        where report waterSupply tile
                | tile == Lava || current == 0 = "You died, GAME OVER"
                | tile == Portal = "Congratulations!" ++ "\n" ++ "Total treasure worth: " ++ show treasureWorth ++ "\n" -- Portal tile, you won!
                | otherwise = "Move!"

-- Start the game
main = runGame DesertExplorerGameConfig { s = 10, m = 10, g = 46, t = makePercentage 50, w = makePercentage 20, p = makePercentage 10, l = makePercentage 5, ll = makePercentage 10 }
