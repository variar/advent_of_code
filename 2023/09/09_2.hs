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

splitToIntegers :: String -> [Integer]
splitToIntegers s = map stringToInteger $ words s
  where
    stringToInteger s = read s :: Integer

adjacentDiff :: [Integer] -> [Integer]
adjacentDiff x = zipWith (-) (drop 1 x) x

predictBack :: [Integer] -> Integer
predictBack d
    | all (==0) d = 0
    | otherwise = head d - predictBack (adjacentDiff d)

solve input = output
    where
        sequences = map splitToIntegers input
        output = sum $ map predictBack sequences
