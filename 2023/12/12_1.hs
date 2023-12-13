import System.Environment
import System.FilePath
import Data.List
import Debug.Trace

main = do
  name <- getProgName
  args <- getArgs
  let inputFile =
        if not (null args)
          then head args
          else take 2 (dropExtension name) ++ "_input"

  input <- readFile inputFile
  print . solve . lines $ input

splitAtDelimiter :: (Char -> Bool) -> String -> [String]
splitAtDelimiter p s = case dropWhile p s of
  "" -> []
  s' -> w : splitAtDelimiter p s''
    where
      (w, s'') = break p s'

splitToIntegers :: String -> [Int]
splitToIntegers s = map stringToInteger $ splitAtDelimiter (==',') s
  where
    stringToInteger s = read s :: Int

type Condition = (String, [Int])
parseConditions :: [String] -> [Condition]
parseConditions = map parseCondition
  where
    parseCondition l = ("#." ++ takeWhile (/=' ') l ++ ".#", [1] ++ (splitToIntegers . dropWhile (/=' ') $ l) ++ [1])

type Pattern = ([Int], [Int])
patternToString :: Pattern -> String
patternToString (damaged, normal) = joinParts normalParts damagedParts
  where
    makeDamaged n = replicate n '#'
    makeNormal n = replicate n '.'
    normalParts = map makeNormal (normal ++ [0])
    damagedParts = map makeDamaged damaged
    joinParts [] [] = []
    joinParts n d = head d ++ head n ++ joinParts (tail n) (tail d)

makePattern :: Condition -> Pattern
makePattern condition = root
  where
    (pattern, damaged) = condition
    totalDamaged = sum damaged
    totalNormal = length pattern - totalDamaged
    requiredNormal = length damaged - 2
    additionalNormal = totalNormal - requiredNormal
    normal = replicate requiredNormal 1 ++ [additionalNormal]
    root = (damaged, normal)

isPossiblePattern :: Condition -> Pattern -> Bool
isPossiblePattern condition pattern = all isMatch pairs
  where
    pairs = zip (fst condition) (patternToString pattern)
    isMatch (c1,c2)
      | c1 == '?' = True
      | otherwise = c1 == c2

makeNormalAlternatives :: [Int] -> [[Int]]
makeNormalAlternatives normal = filter isValid combinations
  where
    totalNormal = sum normal
    partsCount = length normal
    possibleParts = replicate partsCount [1..(totalNormal-partsCount+1)]
    isValid c = sum c == totalNormal
    combinations = sequence possibleParts

countPermutations :: Condition -> Int
countPermutations condition = length possibleAlternatives
  where
    c = traceShowId condition
    p = makePattern condition
    alternatives = makeNormalAlternatives $ snd p
    constructPattern normals = (fst p, normals)
    possibleAlternatives = nub $ filter (isPossiblePattern condition) $ map constructPattern alternatives

solve input = output
    where
        conditions = parseConditions input
        output = sum $ map countPermutations conditions
