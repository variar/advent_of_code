import Data.Char (digitToInt, isDigit)

splitAtDelimiter :: (Char -> Bool) -> String -> [String]
splitAtDelimiter p s = case dropWhile p s of
  "" -> []
  s' -> w : splitAtDelimiter p s''
    where
      (w, s'') = break p s'

stringToInt s = read s :: Integer

tooManyCubesByColor :: [String] -> Bool
tooManyCubesByColor ("red" : xs) = stringToInt (head xs) > 12
tooManyCubesByColor ("green" : xs) = stringToInt (head xs) > 13
tooManyCubesByColor ("blue" : xs) = stringToInt (head xs) > 14
tooManyCubesByColor [] = False

gameId :: String -> Integer
gameId ('G':'a':'m':'e':' ': xs) = stringToInt xs

tooManyCubes :: [String] -> Bool
tooManyCubes (x:xs) = tooManyCubesByColor (reverse (splitAtDelimiter (== ' ') x)) || tooManyCubes xs
tooManyCubes [] = False

acceptGame :: [String] -> Bool
acceptGame (x:xs) = not (tooManyCubes (splitAtDelimiter (== ',') x)) && acceptGame xs
acceptGame [] = True

gameScore :: String -> Integer
gameScore gs = if acceptGame (splitAtDelimiter (==';') (head gameData)) then gameId gameHeader else 0
    where
        (gameHeader:gameData) = splitAtDelimiter (==':') gs

totalScore :: [String] -> Integer
totalScore games = sum (map gameScore games)

printScore :: [String] -> [Char]
printScore s = show (totalScore s) ++ "\n"

main = interact (printScore . lines)