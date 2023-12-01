import Data.Char (isDigit, digitToInt)
count s = show (length s) ++ "\n"

stringsToDigits :: String -> String
stringsToDigits ('o':'n':'e':xs) = '1' : stringsToDigits ('n':'e':xs)
stringsToDigits ('t':'w':'o':xs) = '2' : stringsToDigits ('w':'o':xs)
stringsToDigits ('t':'h':'r':'e':'e':xs) = '3' : stringsToDigits ('h':'r':'e':'e':xs)
stringsToDigits ('f':'o':'u':'r':xs) = '4' : stringsToDigits ('o':'u':'r':xs)
stringsToDigits ('f':'i':'v':'e':xs) = '5' : stringsToDigits ('i':'v':'e':xs)
stringsToDigits ('s':'i':'x':xs) = '6' : stringsToDigits ('i':'x':xs)
stringsToDigits ('s':'e':'v':'e':'n':xs) = '7' : stringsToDigits ('e':'v':'e':'n':xs)
stringsToDigits ('e':'i':'g':'h':'t':xs) = '8' : stringsToDigits ('i':'g':'h':'t':xs)
stringsToDigits ('n':'i':'n':'e':xs) = '9' : stringsToDigits ('i':'n':'e':xs)
stringsToDigits (x:xs)       = x : stringsToDigits xs
stringsToDigits ""           = ""

onlyDigits :: String -> String
onlyDigits =  filter isDigit

calibrationValue :: String -> Int
calibrationValue line = 10 * digitToInt (head d) + digitToInt (last d)
    where
    d = onlyDigits (stringsToDigits line)

lineCalibrations = map calibrationValue


lineCalibrationsStrings = map stringsToDigits

totalCalibration :: [String] -> [Char]
totalCalibration s = show (sum (lineCalibrations s)) ++ "\n"

testCalibration :: [String] -> [Char]
testCalibration s = show (lineCalibrationsStrings s) ++ "\n"

main = interact (totalCalibration . lines)