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


isEmptyRow :: [String] -> Int -> Bool
isEmptyRow input row = all (== '.') (input !! row)

isEmptyColumn :: [String] -> Int -> Bool
isEmptyColumn input column = all (== '.') columnData
  where
    columnAt n s = s !! n
    columnData = map (columnAt column) input

isGalaxy :: String -> Int -> Bool
isGalaxy input pos = input !! pos == '#'

type Universe = (Int, [Int], [Int], [Int])

parseUniverse :: [String] -> Universe
parseUniverse input = (w, galaxies, emptyRows, emptyColumns)
  where
    w = length $ head input
    emptyRows = filter (isEmptyRow input) [0 .. length input - 1]
    emptyColumns = filter (isEmptyColumn input) [0 .. w - 1]
    totalInput = concat input
    galaxies = filter (isGalaxy totalInput) [0 .. length totalInput - 1]

between :: Int->Int->Int->Bool
between lo hi v = v>lo && v<hi

distance :: Universe -> Int -> Int -> Int
distance universe g1 g2 = abs(rowG1 - rowG2) + abs(columnG1 - columnG2) + length emptyRowsBetween + length emptyColumnsBetween
  where
    (w, galaxies, emptyRows, emptyColumns) = universe
    (rowG1,columnG1) = g1 `divMod` w
    (rowG2,columnG2) = g2 `divMod` w
    sortedRows = sort [rowG1, rowG2]
    sortedColumns = sort [columnG1, columnG2]
    emptyRowsBetween = filter (between (head sortedRows) (last sortedRows)) emptyRows
    emptyColumnsBetween = filter (between (head sortedColumns) (last sortedColumns)) emptyColumns

allDistance :: Universe -> [Int] -> Int
allDistance universe [] = 0
allDistance universe galaxies = d + allDistance universe otherG
  where 
    headG = head galaxies
    otherG = tail galaxies
    d 
      | null otherG = 0
      | otherwise = sum $ map (distance universe headG) otherG

solve input = output
  where
    universe = parseUniverse input
    (w, galaxies, emptyRows, emptyColumns) = universe
    output = allDistance universe galaxies