import Debug.Trace (traceShowId)
import Numeric (readHex)
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

data Command = Command {cDir :: Direction, cMove :: Int} deriving (Show)

parseCommand :: String -> Command
parseCommand input = Command dir move
  where
    notIgnoreChar c = c /= '#' && c /= '(' && c /= ')'
    hexData = filter notIgnoreChar (last (words input))
    dir
      | last hexData == '0' = East
      | last hexData == '1' = South
      | last hexData == '2' = West
      | last hexData == '3' = North
    move = (fst . head . readHex) (take 5 hexData)

parseInput :: [String] -> [Command]
parseInput = map parseCommand

data Edge = Edge {eFrom :: (Int, Int), eTo :: (Int, Int)} deriving (Show)

digPath :: [Edge] -> Command -> [Edge]
digPath edges (Command d m)  = edges ++ [e]
  where
    (x0, y0)
      | null edges = (0, 0)
      | otherwise = eTo (last edges)
    (x1, y1)
      | d == East = (x0 + m, y0)
      | d == South = (x0, y0 + m)
      | d == West = (x0 - m, y0)
      | d == North = (x0, y0 - m)
    e = Edge (x0, y0) (x1, y1)

digAllPath :: [Command] -> [Edge]
digAllPath = foldl digPath []

edgeArea :: Edge -> Int
edgeArea (Edge (x0, y0) (x1, y1))
  | x1 == x0 = (y0 - y1) * x1
  | otherwise = (x1 - x0) * y1

edgeLength :: Edge -> Int
edgeLength (Edge (x0, y0) (x1, y1)) = abs (x1 - x0) + abs (y1 - y0)



solve input = output
  where
    commands = parseInput input
    edges = digAllPath commands
    internalArea = abs (sum (map edgeArea edges)) `div` 2
    perimeter = sum $ map edgeLength edges
    output = 1 + internalArea + perimeter `div` 2