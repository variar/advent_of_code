import Data.Char (isDigit)

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

type Range = (Integer, Integer)

parseSeedsRanges :: [Integer] -> [Range]
parseSeedsRanges [] = []
parseSeedsRanges ranges = (head ranges, ranges !! 1) : parseSeedsRanges (drop 2 ranges)

parseSeeds :: String -> [Range]
parseSeeds s = parseSeedsRanges (splitToNumbers (last (splitAtDelimiter (== ':') s)))

type MapEntry = (Range, Range)

type RangeMap = [MapEntry]

parseMapEntry :: String -> MapEntry
parseMapEntry s = ((from, size), (to, size))
  where
    r = splitToNumbers s
    to : from : _ = r
    size = last r

parseRangeMap :: [String] -> (RangeMap, [String])
parseRangeMap lines = (map parseMapEntry rangeStrings, nextPart)
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

intersectRanges :: Range -> Range -> ([Range], [Range])
intersectRanges (a, al) (b, bl)
  | b + bl <= a = ([], [(b, bl)])
  | b >= a + al = ([], [(b, bl)])
  | b < a && b + bl < a + al = ([(a, bl - (a - b))], [(b, a - b)])
  | b < a && b + bl == a + al = ([(a, al)], [(b, a - b)])
  | b < a && b + bl > a + al = ([(a, al)], [(b, a - b), (a + al, b + bl - (a + al))])
  | b == a && b + bl <= a + al = ([(a, bl)], [])
  | b == a && b + bl > a + al = ([(a, al)], [(a + al, b + bl - (a + al))])
  | b > a && b + bl <= a + al = ([(b, bl)], [])
  | b > a && b + bl > a + al = ([(b, a + al - b)], [(a + al, b + bl - (a + al))])

offsetRange :: Range -> Range -> Range -> Range
offsetRange from to input = (fst to + offset, size)
  where
    offset = fst input - fst from
    size = snd input

mapRange :: Range -> MapEntry -> ([Range], [Range])
mapRange input mapEntry = (map (offsetRange from to) mapped, unmapped)
  where
    (from, to) = mapEntry
    (mapped, unmapped) = intersectRanges from input

mapWithRangeMap :: RangeMap -> Range -> [Range]
mapWithRangeMap [] input = [input]
mapWithRangeMap ranges input = mapped ++ mapRest
  where
    (mapped, unmapped) = mapRange input (head ranges)
    mapRest = concatMap (mapWithRangeMap (tail ranges)) unmapped

applyMaps :: [RangeMap] -> Range -> [Range]
applyMaps [] input = [input]
applyMaps rangeMaps input = concatMap (applyMaps (tail rangeMaps)) mapped
  where
    mapped = mapWithRangeMap (head rangeMaps) input

toRangeStarts :: [Range] -> [Integer]
toRangeStarts [] = []
toRangeStarts ranges = fst (head ranges) : toRangeStarts (tail ranges)

printOutput input = show (output) ++ "\n"
  where
    seeds = parseSeeds (head input)
    rangeMaps = parseMaps (tail input)
    locations = concatMap (applyMaps rangeMaps) seeds
    output = (minimum . toRangeStarts) locations

main = interact (printOutput . lines)
