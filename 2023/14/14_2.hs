import Data.List
import Data.Maybe (fromJust)
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

moveRocksLeft :: PlatformSegment -> PlatformSegment
moveRocksLeft segment = intercalate "#" movedGroups
  where
    groups = movableGroups segment
    movedGroups = map sort groups

moveRocksUp :: PlatformState -> PlatformState
moveRocksUp state = map (moveRocksLeft . reverse) (transpose state)

weightRocksLeft :: PlatformSegment -> Int
weightRocksLeft segment = sum $ map succ rockPositions
  where
    rockPositions = elemIndices 'O' segment

weightRocksUp :: PlatformState -> Int
weightRocksUp segment = sum $ map weightRocksLeft linesToWeight
  where
    linesToWeight = map reverse (transpose segment)

periodicMatchLength :: [Int] -> [Int] -> Int
periodicMatchLength sequence pattern
  | length sequence < patternLength = 0
  | take patternLength sequence == pattern = patternLength + periodicMatchLength (drop patternLength sequence) pattern
  | otherwise = 0
  where
    patternLength = length pattern

guessPeriod :: [Int] -> (Int, Int)
guessPeriod sample = (period, aperiodicPrefixLength)
  where
    nPrefixMatchLength sequence n = periodicMatchLength sequence (take n sequence)
    matchedLengths = map (nPrefixMatchLength $ reverse sample) [1 ..]
    maximumMatch = maximum (take 100 matchedLengths)
    period = fromJust (elemIndex maximumMatch matchedLengths) + 1
    aperiodicPrefixLength = length sample - maximumMatch + 1

solve input = output
  where
    movedRocks = iterate moveRocksUp input

    doCycle n = movedRocks !! (4 * n)
    weights = map (weightRocksUp . doCycle) [1 ..]

    cycles = 1000000000
    sampleSize = 250 -- reasonable guess

    sample = take sampleSize weights
    (period, start) = guessPeriod sample
    (d, r) = divMod (cycles - start - 1) period

    aperiodic = take start sample
    periodic = take period $ drop start sample

    output = periodic !! r