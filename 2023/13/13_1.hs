import System.Environment
import System.FilePath

import Data.List

main = do
  name <- getProgName
  args <- getArgs
  let inputFile =
        if not (null args)
          then head args
          else take 2 (dropExtension name) ++ "_input"

  input <- readFile inputFile
  print . solve . lines $ input

parseInputs :: [String] -> [[String]]
parseInputs [] = []
parseInputs lines = [headInput] ++ parseInputs tailInput
  where
    notEmpty l = not (null l)
    headInput = takeWhile (notEmpty) lines
    tailInput = drop (length headInput + 1) lines

isHorizontalMirror :: [String] -> Bool
isHorizontalMirror lines
  | null lines = False
  | odd $ length lines = False
  | otherwise = take middle lines == reverse (drop middle lines)
  where
    middle = length lines `div` 2

horizontalMirror :: Int -> [String] -> Int
horizontalMirror offset lines
  | offset == length lines = 0
  | isHorizontalMirror (drop offset lines) = offset + (length lines - offset) `div` 2
  | otherwise = horizontalMirror (offset + 1) lines

maxHorizontalMirror :: [String] -> Int
maxHorizontalMirror lines = max m1 m2
  where
    m1 = horizontalMirror 0 lines
    reverseM = horizontalMirror 0 (reverse lines)
    m2 = if reverseM == 0 then 0 else length lines - reverseM

maxVerticalMirror :: [String] -> Int
maxVerticalMirror lines = maxHorizontalMirror (transpose lines)

solve input = output
  where
    images = parseInputs input
    vRef = map maxVerticalMirror images
    hRef = map maxHorizontalMirror images
    output = sum vRef + 100 * sum hRef