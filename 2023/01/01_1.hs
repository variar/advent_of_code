import Data.Char (isDigit, digitToInt)
count s = show (length s) ++ "\n"

toDigits :: String -> String
toDigits =  filter isDigit

calibrationValue :: String -> Int
calibrationValue line = 10 * digitToInt (head d) + digitToInt (last d)
    where
    d = toDigits line

lineCalibrations = map calibrationValue

totalCalibration :: [String] -> [Char]
totalCalibration s = show (sum (lineCalibrations s)) ++ "\n"

main = interact (totalCalibration . lines)