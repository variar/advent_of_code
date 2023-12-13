import Data.List
import Data.Maybe
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
  print ("Result:", solve . lines $ input)

splitAtDelimiter :: (Char -> Bool) -> String -> [String]
splitAtDelimiter p s = case dropWhile p s of
  "" -> []
  s' -> w : splitAtDelimiter p s''
    where
      (w, s'') = break p s'

splitToIntegers :: String -> [Int]
splitToIntegers s = map stringToInteger $ splitAtDelimiter (== ',') s
  where
    stringToInteger s = read s :: Int

type Condition = (String, [Int])

addCorners :: Condition -> Condition
addCorners (pattern, damaged) = ("#." ++ pattern ++ ".#", [1] ++ damaged ++ [1])

unfoldCondition :: String -> Int -> Condition -> Condition
unfoldCondition c folds (pattern, damaged) = (intercalate c $ replicate folds pattern, concat $ replicate folds damaged)

parseConditions :: String -> Int -> [String] -> [Condition]
parseConditions c folds = map (unfoldCondition c folds . parseCondition)
  where
    parseCondition l = (takeWhile (/= ' ') l, (splitToIntegers . dropWhile (/= ' ') $ l))

extractDamaged :: String -> [Int]
extractDamaged pattern = map length headParts
  where
    parts = splitAtDelimiter (== '?') pattern
    headParts = splitAtDelimiter (== '.') (head parts)

isPossiblePattern :: Condition -> String -> Bool
isPossiblePattern condition pattern
  | leftUnknown == 0 = damagedHead == damaged
  | otherwise = all isMatch pairsHead && all isMatch pairsTail
  where
    damagedHead = extractDamaged pattern
    damagedTail = extractDamaged $ reverse pattern
    unknownDamaged = length damaged - (length damagedHead + length damagedTail)
    leftUnknown = length $ filter (== '?') pattern
    damaged = snd condition
    pairsHead = zip damagedHead damaged
    pairsTail = zip damagedTail (reverse damaged)
    isMatch (c1, c2)
      | c1 == 0 || c2 == 0 = False
      | c1 == c2 = True
      | leftUnknown > 0 && c1 < c2 = True
      | otherwise = False

type PossiblePatternPredicate = String -> Bool

replaceOne :: PossiblePatternPredicate -> String -> [String]
replaceOne checkFn pattern = filter checkFn newPatterns
  where
    marks = elemIndices '?' pattern
    addChar c m p = take (m) p ++ [c] ++ drop (m + 1) p
    newPatterns
      | null marks = [pattern]
      | length marks == 1 = step1
      | otherwise = map (addChar '.' (head marks)) step1 ++ map (addChar '#' (head marks)) step1
      where
        step1 = [addChar '.' (last marks) pattern, addChar '#' (last marks) pattern]

countPossiblePatterns :: PossiblePatternPredicate -> String -> Int
countPossiblePatterns checkFn pattern
  | null nextPatterns = 0
  | head nextPatterns == pattern = 1 + 0 * length (pattern)
  | otherwise = sum $ map (countPossiblePatterns checkFn) nextPatterns
  where
    nextPatterns = replaceOne checkFn pattern

countPossiblePatternsForCondition :: Condition -> Int
countPossiblePatternsForCondition condition = traceShowId c
  where
    corners = addCorners condition
    c = countPossiblePatterns (isPossiblePattern corners) (fst $ traceShowId corners)

tryDot :: PossiblePatternPredicate -> String -> Maybe (String, String)
tryDot checkFn pattern = newPatterns
  where
    marks = elemIndices '!' pattern
    middleMark = take (length marks `div` 2 + 1) marks
    split m p = (take m p ++ ".#", "#." ++ drop (m + 1) p)
    newPatterns
      | null marks = Nothing
      | otherwise = Just (split (head middleMark) pattern)

isValidCondition :: Condition -> Bool
isValidCondition c = length (fst c) >= sum (snd c) + length (snd c) - 1

makeSmallerConditions :: Condition -> [(Condition, Condition)]
makeSmallerConditions condition = filter isValidSubConditions subConditions
  where
    damaged = snd condition
    corners = addCorners condition
    subPatterns = fromJust (tryDot (isPossiblePattern corners) (fst corners))
    splitDamaged n = ([1] ++ take (length damaged - n) damaged ++ [1], [1] ++ drop (length damaged - n) damaged ++ [1])
    splittedDamaged = map splitDamaged [0..length damaged]
    buildCondition p d = ((fst p, fst d), (snd p,  snd d))
    subConditions = map (buildCondition subPatterns) splittedDamaged
    isValidSubConditions (c1, c2) = isValidCondition c1 && isValidCondition c2

countPossiblePatternsFromSubConditions :: Condition -> Int
countPossiblePatternsFromSubConditions condition = traceShowId c
  where
    corneredCondition = addCorners condition
    subConditions = makeSmallerConditions corneredCondition
    countSubConditions (c1, c2) = countPossiblePatternsForCondition c1 * countPossiblePatternsForCondition c2
    c = sum $ map countSubConditions subConditions

solve input = output
  where
    conditions = parseConditions "?" 2 input
    conditions1 = parseConditions "!" 2 input
    output = sum $ map countPossiblePatternsForCondition [conditions!!1]
    -- output = sum $ map countPossiblePatternsFromSubConditions [conditions1!!1]
