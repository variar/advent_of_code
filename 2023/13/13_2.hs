import Control.Applicative.Backwards (Backwards (forwards))
import Data.List
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

horizontalMirrorsAt :: Int -> [String] -> [Int]
horizontalMirrorsAt offset lines
  | offset == length lines = []
  | isHorizontalMirror (drop offset lines) = (offset + (length lines - offset) `div` 2) : horizontalMirrorsAt (offset + 1) lines
  | otherwise = horizontalMirrorsAt (offset + 1) lines

allHorizontalMirrors :: [String] -> [Int]
allHorizontalMirrors lines = sort $ nub (forwardM ++ reverseM)
  where
    forwardM = (horizontalMirrorsAt 0 lines)
    reverseMRaw = (horizontalMirrorsAt 0 (reverse lines))
    fixReversM m = if m == 0 then 0 else length lines - m
    reverseM = map fixReversM reverseMRaw

allVerticalMirrors :: [String] -> [Int]
allVerticalMirrors lines = allHorizontalMirrors (transpose lines)

anyMirrorChanges :: [String] -> [String] -> Bool
anyMirrorChanges image fixedImage = isChange
  where
    originalHMirrors = allHorizontalMirrors image
    originalVMirrors = allVerticalMirrors image
    fixedHMirrors = allHorizontalMirrors fixedImage
    fixedVMirrors = allVerticalMirrors fixedImage
    isChange
      | not (null originalHMirrors) = (not (null fixedHMirrors) && originalHMirrors /= fixedHMirrors) || not (null fixedVMirrors)
      | otherwise = (not (null fixedVMirrors) && originalVMirrors /= fixedVMirrors) || not (null fixedHMirrors)

allMirrorChanges :: [String] -> [String] -> Bool
allMirrorChanges image fixedImage = isChange
  where
    originalHMirrors = allHorizontalMirrors image
    originalVMirrors = allVerticalMirrors image
    fixedHMirrors = allHorizontalMirrors fixedImage
    fixedVMirrors = allVerticalMirrors fixedImage
    isChange
      | not (null originalHMirrors) = (not (null fixedHMirrors) && notElem (head originalHMirrors) fixedHMirrors) || not (null fixedVMirrors)
      | otherwise = (not (null fixedVMirrors) && notElem (head originalVMirrors) fixedVMirrors) || not (null fixedHMirrors)

fixSmudge :: Int -> Int -> [String] -> [String]
fixSmudge row col lines = prefix ++ [fixedLine] ++ (tail suffix)
  where
    prefix = take row lines
    suffix = drop row lines
    lineWithSmudge = head suffix
    fixSymbol c
      | c == '.' = '#'
      | otherwise = '.'
    linePrefix = take col lineWithSmudge
    lineSuffix = drop (col + 1) lineWithSmudge
    fixedLine = linePrefix ++ [fixSymbol (lineWithSmudge !! col)] ++ lineSuffix

fixSmudgeAt :: [String] -> Int -> [String]
fixSmudgeAt image smudgePos = fixSmudge row col image
  where
    (row, col) = smudgePos `divMod` length (head image)

hasSmudgeAt :: [String] -> Int -> Bool
hasSmudgeAt image pos = anyMirrorChanges image (fixSmudgeAt image pos)

allSmudgePositions :: [String] -> [Int]
allSmudgePositions lines = filter (hasSmudgeAt lines) [0 .. length lines * length (head lines) - 1]

allPossibleSmudgeFixes :: [String] -> [[String]]
allPossibleSmudgeFixes image = map (fixSmudgeAt image) (allSmudgePositions image)

fixCorrectSmudge :: [String] -> [String]
fixCorrectSmudge image = bestSmudgeFix
  where
    hMirrors = allHorizontalMirrors image
    vMirrors = allVerticalMirrors image
    smudgeFixes = allPossibleSmudgeFixes image
    isFullChange = allMirrorChanges image
    fullChangeSmudges = filter isFullChange smudgeFixes
    bestSmudgeFix
      | not (null fullChangeSmudges) = head fullChangeSmudges
      | otherwise = head smudgeFixes

score :: [String] -> Int
score image = bestScore
  where
    fixedImage = fixCorrectSmudge image
    originalHMirrors = allHorizontalMirrors image
    originalVMirrors = allVerticalMirrors image
    fixedHMirrors = allHorizontalMirrors fixedImage \\ originalHMirrors
    fixedVMirrors = allVerticalMirrors fixedImage \\ originalVMirrors
    bestScore
      | not (null fixedHMirrors) = head fixedHMirrors * 100
      | otherwise = head fixedVMirrors

solve input = output
  where
    images = parseInputs input
    output = sum $ map score images
