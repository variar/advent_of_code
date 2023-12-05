import Data.Char (isDigit)
import Data.Maybe

splitAtDelimiter :: (Char -> Bool) -> String -> [String]
splitAtDelimiter p s = case dropWhile p s of
  "" -> []
  s' -> w : splitAtDelimiter p s''
    where
      (w, s'') = break p s'

splitToNumbers :: String -> [Integer]
splitToNumbers s = map stringToInt (splitAtDelimiter (== ' ') s)
  where
    stringToInt s = read s :: Integer

parseSeeds :: String -> [Integer]
parseSeeds s = splitToNumbers (last (splitAtDelimiter (== ':') s))

type Range = (Integer, Integer, Integer)

type RangeMap = [Range]

parseRange :: String -> Range
parseRange s = (r !! 0, r !! 1, r !! 2)
  where
    r = splitToNumbers s

parseRangeMap :: [String] -> (RangeMap, [String])
parseRangeMap lines = (map parseRange rangeStrings, nextPart)
  where
    startsWithDigit s = not (null s) && isDigit (head s)
    skippedHeader = drop 2 lines
    rangeStrings = takeWhile startsWithDigit skippedHeader
    nextPart = dropWhile startsWithDigit skippedHeader

parseMaps :: [String] -> [RangeMap]
parseMaps [] = []
parseMaps ranges = rangeMap : parseMaps next
  where
    (rangeMap, next) = parseRangeMap ranges

tryMapWithRange :: Integer -> Range -> Maybe Integer
tryMapWithRange n range = if inRange then Just (outBegin + offset) else Nothing
  where
    (outBegin, inBegin, rangeLength) = range
    inRange = n >= inBegin && n < inBegin + rangeLength
    offset = n - inBegin

mapWithRangeMap :: Integer -> RangeMap -> Integer
mapWithRangeMap n [] = n
mapWithRangeMap n ranges = if isMapped then fromJust maybeMapped else mapWithRangeMap n (tail ranges)
  where
    maybeMapped = tryMapWithRange n (head ranges)
    isMapped = isJust maybeMapped

applyMaps :: [RangeMap] -> Integer -> Integer
applyMaps [] n = n
applyMaps rangeMaps n = applyMaps (tail rangeMaps) mapped
  where
    mapped = mapWithRangeMap n (head rangeMaps)

printOutput input = show output ++ "\n"
  where
    seeds = parseSeeds (head input)
    rangeMaps = parseMaps (tail input)
    locations = map (applyMaps rangeMaps) seeds
    output = minimum locations

main = interact (printOutput . lines)
