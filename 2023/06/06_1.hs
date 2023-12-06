splitAtDelimiter :: (Char -> Bool) -> String -> [String]
splitAtDelimiter p s = case dropWhile p s of
  "" -> []
  s' -> w : splitAtDelimiter p s''
    where
      (w, s'') = break p s'

splitToNumbers :: String -> [Float]
splitToNumbers s = map stringToFloat (splitAtDelimiter (== ' ') s)
  where
    stringToFloat s = read s :: Float

parseRaces :: (String, String) -> [(Float, Float)]
parseRaces (t, d) = zip (splitToNumbers times) (splitToNumbers distances)
    where
        times = last (splitAtDelimiter (== ':') t)
        distances = last (splitAtDelimiter (== ':') d)

waysToWinRace :: (Float, Float) -> Integer
waysToWinRace (t, r) = upper - lower + 1 + du + dl
    where
        d = sqrt(0.25*t*t - r)
        x1 = -0.5*t - d
        x2 = -0.5*t + d
        upper = floor x2 :: Integer
        lower = ceiling x1 :: Integer
        du = if abs(fromIntegral upper - x2) < 0.000001 then -1 else 0
        dl = if abs(fromIntegral lower - x1) < 0.000001 then -1 else 0

printOutput input = show (output) ++ "\n"
  where
    races = parseRaces (head input, last input)
    output = product (map waysToWinRace races)

main = interact (printOutput . lines)