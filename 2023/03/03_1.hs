import Data.Char (digitToInt, isDigit)

engineWidth :: String -> Int
engineWidth s = length (head (lines s))

digitsToInt :: [Int] -> Int -> Int
digitsToInt (x : xs) n = x * 10 ^ n + digitsToInt xs (n + 1)
digitsToInt [] n = 0

isSymbol :: Char -> Bool
isSymbol '.' = False
isSymbol c = not (isDigit c)

notDigit :: Char -> Bool
notDigit c = not (isDigit c)

isSymbolAt :: String -> Int -> Bool
isSymbolAt fullEngineDoc n = (n >= 0 && n < length fullEngineDoc) && isSymbol (fullEngineDoc !! n)

adjacentPositions :: Int -> Int -> [Int]
adjacentPositions w n = [n - 1, n + 1, n - w, n + w, n - w + 1, n - w - 1, n + w + 1, n + w - 1]

isAdjacentToSymbolAt :: String -> Int -> Int -> Bool
isAdjacentToSymbolAt fullEngineDoc w n = any (isSymbolAt fullEngineDoc) (adjacentPositions w n)

isAdjacentToSymbol :: String -> Int -> (Int, Int) -> Bool
isAdjacentToSymbol fullEngineDoc w (l, r) = any (isAdjacentToSymbolAt fullEngineDoc w) [l .. r]

extractNumber :: ((Int, Int) -> Bool) -> Int -> String -> (Int, Int, String)
extractNumber isPartPredicate offset partialEngineDoc = (digitsToInt (reverse digits) 0, r + 1, afterDigits)
  where
    nonDigits = takeWhile notDigit partialEngineDoc
    skippedNonDigits = dropWhile notDigit partialEngineDoc
    digitsString = takeWhile isDigit skippedNonDigits
    afterDigits = dropWhile isDigit skippedNonDigits
    l = offset + length nonDigits
    r = l + length digitsString - 1
    digits = if isPartPredicate (l, r) then map digitToInt digitsString else []

partNumbers :: ((Int, Int) -> Bool) -> Int -> String -> [Int]
partNumbers isPartPredicate offset partialEngineDoc = first : (if not (null nextEngineDoc) then partNumbers isPartPredicate nextOffset nextEngineDoc else [])
  where
    (first, nextOffset, nextEngineDoc) = extractNumber isPartPredicate offset partialEngineDoc

finalPartNumber :: ((Int, Int) -> Bool) -> String -> Int
finalPartNumber isPartPredicate engineDoc = sum (partNumbers isPartPredicate 0 engineDoc)

printPart :: String -> [Char]
printPart s = show (finalPartNumber isPartPredicate engineDoc) ++ "\n"
  where
    engineDoc = filter (/= '\n') s
    w = engineWidth s
    isPartPredicate = isAdjacentToSymbol engineDoc w

main = interact printPart