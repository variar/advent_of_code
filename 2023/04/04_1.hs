import Data.ByteString (count)
splitAtDelimiter :: (Char -> Bool) -> String -> [String]
splitAtDelimiter p s = case dropWhile p s of
  "" -> []
  s' -> w : splitAtDelimiter p s''
    where
      (w, s'') = break p s'

stringToInt s = read s :: Integer

splitToNumbers :: String -> [Integer]
splitToNumbers s = map stringToInt (splitAtDelimiter (==' ') s)

parseCard :: String -> ([Integer], [Integer])
parseCard s = (splitToNumbers leftSide, splitToNumbers rightSide)
    where
        numbersPart = last (splitAtDelimiter (==':') s)
        sides = splitAtDelimiter (=='|') numbersPart
        (leftSide, rightSide) = (head sides, last sides)

isWinningNumber :: [Integer]->Integer->Integer
isWinningNumber winning n = if n `elem` winning then 1 else 0

countWinningNumbers :: [Integer]->[Integer]->Integer
countWinningNumbers numbers winning = sum (map (isWinningNumber winning) numbers)

score :: Integer -> Integer
score 0 = 0
score 1 = 1
score n = 2 * score (n-1)

cardScore :: String -> Integer
cardScore card = score (countWinningNumbers winningNumbers numbers)
    where 
        (numbers, winningNumbers) = parseCard card

totalScore :: [String] -> Integer
totalScore cards = sum (map cardScore cards)

printScore :: [String] -> [Char]
printScore s = show (totalScore s) ++ "\n"

main = interact (printScore . lines)

