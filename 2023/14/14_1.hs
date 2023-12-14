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

type PlatformSegment = String

type PlatformState = [PlatformSegment]

movableGroups :: PlatformSegment -> [PlatformSegment]
movableGroups line
  | null unmovable = [line]
  | otherwise = newGroup : movableGroups leftover
  where
    unmovable = elemIndices '#' line
    newGroup = take (head unmovable) line
    leftover = drop (head unmovable + 1) line

moveRocks :: PlatformSegment -> PlatformSegment
moveRocks segment = intercalate "#" movedGroups
  where
    groups = movableGroups segment
    movedGroups = map sort groups

weightRocks :: PlatformSegment -> Int
weightRocks segment = sum $ map succ rockPositions
  where
    rockPositions = elemIndices 'O' segment

solve input = output
  where
    movedRocks = map (moveRocks . reverse) (transpose input)
    output = sum $ map weightRocks movedRocks