{-# LANGUAGE MultiWayIf #-}

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace
import System.Environment
import System.FilePath

main = do
  name <- getProgName
  args <- getArgs
  let inputFile =
        if not (null args)
          then head args
          else take 2 (dropExtension name) ++ "_input"

  input <- readFile inputFile
  print . solve . lines $ input

data Direction = N | E | W | S deriving (Eq, Show)

data Tile = ET | WT | MJ | ML | HS | VS deriving (Eq, Show)

data Layout = Layout {layoutWidth :: Int, layoutTiles :: [Tile]} deriving (Show)

newtype EnergizedTile = EnergizedTile {energizedDirections :: [Direction]} deriving (Show)

type EnergizedLayout = Map Int EnergizedTile

parseInput :: [String] -> Layout
parseInput lines = Layout w tiles
  where
    w = length (head lines) + 2
    parseTile c
      | c == '.' = ET
      | c == '/' = MJ
      | c == '\\' = ML
      | c == '|' = VS
      | c == '-' = HS
      | c == '#' = WT
    wallLine = replicate w '#'
    addWalls l = ['#'] ++ l ++ ['#']
    linesWithWalls = map addWalls lines
    tiles = map parseTile (wallLine ++ concat linesWithWalls ++ wallLine)

data DirectionalPosition = DirectionalPosition {posDir :: Direction, posPos :: Int} deriving (Show)

wasEnergizedInDirection :: EnergizedLayout -> DirectionalPosition -> Bool
wasEnergizedInDirection energizedTiles (DirectionalPosition d p)
  | isJust maybeEnergized = wasEnergized (fromJust maybeEnergized) d
  | otherwise = False
  where
    maybeEnergized = Map.lookup p energizedTiles
    wasEnergized (EnergizedTile directions) d = d `elem` directions

moveNext :: Layout -> DirectionalPosition -> [DirectionalPosition]
moveNext layout pos = map moveInDirection nextDirections
  where
    (Layout w tiles) = layout
    (DirectionalPosition d p) = pos
    moveInDirection N = DirectionalPosition N (p - w)
    moveInDirection S = DirectionalPosition S (p + w)
    moveInDirection E = DirectionalPosition E (p + 1)
    moveInDirection W = DirectionalPosition W (p - 1)
    tileAtPos = tiles !! p
    nextDirections
      | tileAtPos == WT = []
      | tileAtPos == ET = [d]
      | tileAtPos == MJ =
          if
            | d == N -> [E]
            | d == E -> [N]
            | d == S -> [W]
            | d == W -> [S]
      | tileAtPos == ML =
          if
            | d == N -> [W]
            | d == E -> [S]
            | d == S -> [E]
            | d == W -> [N]
      | tileAtPos == HS =
          if
            | d == E || d == W -> [d]
            | d == N || d == S -> [E, W]
      | tileAtPos == VS =
          if
            | d == N || d == S -> [d]
            | d == E || d == W -> [N, S]

energizeInDirection :: EnergizedLayout -> DirectionalPosition -> EnergizedLayout
energizeInDirection energizedTiles pos
  | isJust maybeEnergized = Map.adjust energize p energizedTiles
  | otherwise = Map.insert p (EnergizedTile [d]) energizedTiles
  where
    (DirectionalPosition d p) = pos
    energize t = EnergizedTile (d : energizedDirections t)
    maybeEnergized = Map.lookup p energizedTiles

propagateBeam :: Layout -> EnergizedLayout -> DirectionalPosition -> EnergizedLayout
propagateBeam layout energizedLayout pos
  | wasEnergizedInDirection energizedLayout pos = energizedLayout
  | null nextMoves = energizedLayout
  | length nextMoves == 1 = energizedLayout1
  | otherwise = energizedLayout2
  where
    (Layout w tiles) = layout
    (DirectionalPosition d p) = pos
    nextMoves = moveNext layout pos
    energizedThis = energizeInDirection energizedLayout pos
    energizedLayout1 = propagateBeam layout energizedThis (head nextMoves)
    energizedLayout2 = propagateBeam layout energizedLayout1 (last nextMoves)

countEnergized :: Layout -> DirectionalPosition -> Int
countEnergized layout start = Map.size $ propagateBeam layout Map.empty start

solve input = output
  where
    layout = parseInput input
    Layout w tiles = layout
    start = DirectionalPosition E (w + 1)
    output = countEnergized layout start
