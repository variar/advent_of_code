import Data.List
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

movableGroups :: String -> [String]
movableGroups line
  | null unmovable = [line]
  | otherwise = next : movableGroups toSplit
  where
    unmovable = elemIndices '#' line
    next = take (head unmovable) line
    toSplit = drop (head unmovable + 1) line

moveRocks :: String -> String
moveRocks line = intercalate "#" movedGroups
  where
    groups = movableGroups line
    movedGroups = map sort groups

weightRocks :: String -> Int
weightRocks line = sum $ zipWith (+) rockPositions (replicate (length rockPositions) 1)
  where
    rockPositions = elemIndices 'O' line

solve input = output
  where
    movedRocks = map (moveRocks . reverse) (transpose input)
    output = sum $ map weightRocks movedRocks