import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
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

type Pattern = String

type DamagedSprings = [Int]

type Condition = (Pattern, DamagedSprings)

addCorners :: Condition -> Condition
addCorners (pattern, damaged) = ("#." ++ pattern ++ ".#", [1] ++ damaged ++ [1])

unfoldCondition :: String -> Int -> Condition -> Condition
unfoldCondition c folds (pattern, damaged) = (intercalate c $ replicate folds pattern, concat $ replicate folds damaged)

parseConditions :: String -> Int -> [String] -> [Condition]
parseConditions c folds = map (addCorners . unfoldCondition c folds . parseCondition)
  where
    parseCondition l = (takeWhile (/= ' ') l, splitToIntegers . dropWhile (/= ' ') $ l)

type MatchPrefixLength = Int

type MatchState = (MatchPrefixLength, Pattern, DamagedSprings)

type MemoMap = Map MatchState Int

countMatchesFromState :: MemoMap -> MatchState -> (MemoMap, Int)
countMatchesFromState memo state
  | isJust knownState = (memo, fromJust knownState)
  | null pattern && length unmatchedDamaged /= 1 = memoize (memo, 0)
  | null pattern && head unmatchedDamaged /= matchedPrefix = memoize (memo, 0)
  | null pattern && head unmatchedDamaged == matchedPrefix = memoize (memo, 1)
  | null unmatchedDamaged = if '#' `notElem` pattern then memoize (memo, 1) else memoize (memo, 0)
  | sum unmatchedDamaged - matchedPrefix > length pattern - length unmatchedDamaged + 1 = memoize (memo, 0)
  | sum unmatchedDamaged - matchedPrefix > length (filter (== '#') pattern) + length (filter (== '?') pattern) = memoize (memo, 0)
  | p == '#' && (matchedPrefix >= head unmatchedDamaged) = memoize (memo, 0)
  | p == '#' && (matchedPrefix < head unmatchedDamaged) = memoize $ countMatchesFromState memo (matchedPrefix + 1, xp, unmatchedDamaged)
  | p == '.' && matchedPrefix == 0 = memoize $ countMatchesFromState memo (matchedPrefix, xp, unmatchedDamaged)
  | p == '.' && matchedPrefix < head unmatchedDamaged = memoize (memo, 0)
  | p == '.' && matchedPrefix == head unmatchedDamaged = memoize $ countMatchesFromState memo (0, xp, tail unmatchedDamaged)
  | p == '?' = memoize (memo2, count1 + count2)
  where
    knownState = Map.lookup state memo
    (matchedPrefix, pattern, unmatchedDamaged) = state
    p : xp = pattern
    (memo1, count1) = countMatchesFromState memo (matchedPrefix, '.' : xp, unmatchedDamaged)
    (memo2, count2) = countMatchesFromState memo1 (matchedPrefix, '#' : xp, unmatchedDamaged)
    memoize (m, v) = (Map.insert state v m, v)

countMatches :: Condition -> Int
countMatches (pattern, damaged) = snd $ countMatchesFromState Map.empty (0, pattern, damaged)

solve input = output
  where
    conditions = parseConditions "?" 5 input
    output = sum $ map countMatches conditions
