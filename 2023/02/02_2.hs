import Data.Char (digitToInt, isDigit)

splitAtDelimiter :: (Char -> Bool) -> String -> [String]
splitAtDelimiter p s = case dropWhile p s of
  "" -> []
  s' -> w : splitAtDelimiter p s''
    where
      (w, s'') = break p s'

stringToInt s = read s :: Integer

gameId :: String -> Integer
gameId ('G':'a':'m':'e':' ': xs) = stringToInt xs

countCubesByColor :: [String] -> [Integer]
countCubesByColor ("red" : xs) = [stringToInt (head xs), 0 , 0]
countCubesByColor ("green" : xs) = [0, stringToInt (head xs), 0]
countCubesByColor ("blue" : xs) = [0 , 0, stringToInt (head xs)]
countCubesByColor [] = [0,0,0]

countCubes :: [String] -> [Integer]
countCubes (x:xs) = zipWith (+) (countCubesByColor (reverse (splitAtDelimiter (== ' ') x))) (countCubes xs)
countCubes [] = [0, 0 ,0]

acceptableGame :: [String] -> [Integer]
acceptableGame (x:xs) = zipWith max (countCubes (splitAtDelimiter (== ',') x)) (acceptableGame xs) 
acceptableGame [] = [0,0,0]

gameScore :: String -> Integer
gameScore gs = product (acceptableGame (splitAtDelimiter (==';') (head gameData)))
    where
        (gameHeader:gameData) = splitAtDelimiter (==':') gs

totalScore :: [String] -> Integer
totalScore games = sum (map gameScore games)

printScore :: [String] -> [Char]
printScore s = show (totalScore s) ++ "\n"

main = interact (printScore . lines)