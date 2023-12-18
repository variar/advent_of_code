import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Debug.Trace (traceShow)
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

data Direction = North | South | East | West deriving (Show, Eq)

data Command = Command {cDir :: Direction, cMove :: Int, cColor :: String} deriving (Show)

parseCommand :: String -> Command
parseCommand input = Command dir move color
  where
    parts = words input
    dir
      | head parts == "U" = North
      | head parts == "D" = South
      | head parts == "L" = West
      | head parts == "R" = East
    move = read (parts !! 1) :: Int
    color = last parts

parseInput :: [String] -> [Command]
parseInput = map parseCommand

digPath :: [(Int, Int)] -> Command -> [(Int, Int)]
digPath path (Command cD cM _) = path ++ newDig
  where
    lastPos = last path
    digInDir d p m
      | d == North = (fst p, snd p - m)
      | d == South = (fst p, snd p + m)
      | d == East = (fst p + m, snd p)
      | d == West = (fst p - m, snd p)
    newDig = map (digInDir cD lastPos) [1 .. cM]

digAllPath :: [Command] -> [(Int, Int)]
digAllPath = foldl digPath [(0, 0)]

boundingBox :: [(Int, Int)] -> (Int, Int, Int, Int)
boundingBox path = (left, top, right, bottom)
  where
    top = minimum (map snd path)
    bottom = maximum (map snd path)
    left = minimum (map fst path)
    right = maximum (map fst path)

offsetPath :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
offsetPath path (oX, oY) = map addOffset path
  where
    addOffset p = (fst p + oX, snd p + oY)

neighbors :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbors path (right, bottom) (x, y) = filter (`notElem` path) insideBox
  where
    possible = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    isInBox (i, j) = i >= 0 && i <= right + 1 && j >= 0 && j <= bottom + 1
    insideBox = filter isInBox possible

type OutsideMap = Map (Int, Int) Bool

fillOutside :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)] -> OutsideMap -> OutsideMap
fillOutside path (right, bottom) toVisit outside
  | null toVisit = outside
  | otherwise = fillOutside path (right, bottom) toFill filledMap
  where
    (p : ps) = toVisit
    filledMap = Map.insert p True outside
    notFilled p = isNothing (Map.lookup p outside)
    toFill = nub (ps ++ filter notFilled (neighbors path (right, bottom) p))

solve input = output
  where
    commands = parseInput input

    path = digAllPath commands
    (_, top, _, _) = boundingBox path

    normalizedPath = offsetPath path (1, -top + 1)
    (_, _, right, bottom) = boundingBox normalizedPath

    filled = fillOutside normalizedPath (right, bottom) [(0, 0)] Map.empty
    output = (right + 2) * (bottom + 2) - length filled