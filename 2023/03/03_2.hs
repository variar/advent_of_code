import Data.Char (digitToInt, isAlphaNum, isDigit)
import Data.List (nub)

engineWidth :: String -> Int
engineWidth engineDoc = length (head (lines engineDoc))

digitsToInt :: [Int] -> Int -> Int
digitsToInt (x : xs) n = x * 10 ^ n + digitsToInt xs (n + 1)
digitsToInt [] n = 0

isGear :: Char -> Bool
isGear = (== '*')

notDigit :: Char -> Bool
notDigit c = not (isDigit c)

isGearAt :: String -> Int -> Bool
isGearAt engineDoc n = (n >= 0 && n < length engineDoc) && isGear (engineDoc !! n)

adjacentPositions :: Int -> Int -> [Int]
adjacentPositions w n = [n - 1, n + 1, n - w, n + w, n - w + 1, n - w - 1, n + w + 1, n + w - 1]

gearPosition :: String -> Int -> Int
gearPosition fullEngineDoc n = if isGearAt fullEngineDoc n then n else -1

adjacentGears :: String -> Int -> Int -> [Int]
adjacentGears fullEngineDoc w n = filter (>= 0) (map (gearPosition fullEngineDoc) (adjacentPositions w n))

allAdjacentGears :: String -> Int -> (Int, Int) -> [Int]
allAdjacentGears fullEngineDoc w (l, r) = nub (concat gearsForN)
  where
    gearsForN = map (adjacentGears fullEngineDoc w) [l .. r]

extractPartWithGears :: ((Int, Int) -> [Int]) -> Int -> String -> (Int, [Int], Int, String)
extractPartWithGears gearsSelector offset partialEngineDoc =
  (digitsToInt (reverse digits) 0, gears, r + 1, afterDigits)
  where
    nonDigits = takeWhile notDigit partialEngineDoc
    skippedNonDigits = dropWhile notDigit partialEngineDoc
    digitsString = takeWhile isDigit skippedNonDigits
    afterDigits = dropWhile isDigit skippedNonDigits
    l = offset + length nonDigits
    r = l + length digitsString - 1
    gears = gearsSelector (l, r)
    digits = if not (null gears) then map digitToInt digitsString else []

allPartNumbersWithGears :: ((Int, Int) -> [Int]) -> Int -> String -> [(Int, [Int])]
allPartNumbersWithGears gearsSelector offset partialEngineDoc =
  (first, gears)
    : ( if not (null nextEngineDoc)
          then allPartNumbersWithGears gearsSelector nextOffset nextEngineDoc
          else []
      )
  where
    (first, gears, nextOffset, nextEngineDoc) = extractPartWithGears gearsSelector offset partialEngineDoc

gearPart :: Int -> (Int, [Int]) -> Int
gearPart g part = if g `elem` snd part then fst part else -1

gearRatio :: [(Int, [Int])] -> Int -> Int
gearRatio parts g = if length gearParts == 2 then product gearParts else 0
  where
    gearParts = filter (> 0) (map (gearPart g) parts)

gearsRatios :: [(Int, [Int])] -> [Int]
gearsRatios parts = ratios ++ if not (null parts) then gearsRatios (tail parts) else []
  where
    gears = if not (null parts) then snd (head parts) else []
    ratios = map (gearRatio parts) gears

extractNumberD :: ((Int, Int) -> [Int]) -> Int -> String -> ([Int], [Int], Int, String)
extractNumberD p d s = (reverse digits, gears, r, afterDigits)
  where
    nonDigits = takeWhile notDigit s
    skippedNonDigits = dropWhile notDigit s
    digitsString = takeWhile isDigit skippedNonDigits
    afterDigits = dropWhile isDigit skippedNonDigits
    l = d + length nonDigits
    r = l + length digitsString - 1
    gears = p (l, r)
    digits = if not (null gears) then map digitToInt digitsString else []

printResult s = show (sum (gearsRatios parts)) ++ "\n"
  where
    fullEngineDoc = filter (/= '\n') s
    w = engineWidth s
    p = allAdjacentGears fullEngineDoc w
    parts = allPartNumbersWithGears p 0 fullEngineDoc

main = interact printResult