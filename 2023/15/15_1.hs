import Data.Char (ord)
import System.Environment
import System.FilePath

main = do
  name <- getProgName
  args <- getArgs
  let inputFile =
        if not (null args)
          then head args
          else take 2 (dropExtension name) ++ "_input"

  input <- readFile inputFile
  print . solve $ input

splitAtDelimiter :: (Char -> Bool) -> String -> [String]
splitAtDelimiter p s = case dropWhile p s of
  "" -> []
  s' -> w : splitAtDelimiter p s''
    where
      (w, s'') = break p s'

hashCommand :: String -> Int
hashCommand = foldl (\c x -> snd ((c * 17 + 17 * ord x) `divMod` 256)) 0

solve input = output
  where
    commands = splitAtDelimiter (== ',') input
    output = sum $ map hashCommand commands