import Data.Char (ord)
import Data.List (find)
import Data.Maybe (isJust)
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

hashLabel :: String -> Int
hashLabel = foldl (\c x -> snd ((c * 17 + 17 * ord x) `divMod` 256)) 0

data Lens = Lens {lenseLabel :: String, focal :: Int} deriving (Show)

data Operation = Insert | Remove deriving (Show)

data Command = Command {commandOp :: Operation, commandLabel :: String, commandFocal :: Int} deriving (Show)

type Box = [Lens]

parseCommand :: String -> Command
parseCommand command
  | '=' `elem` command = Command Insert label (read (last focal) :: Int)
  | '-' `elem` command = Command Remove label 0
  where
    isCommandChar c = c == '=' || c == '-'
    (label : focal) = splitAtDelimiter isCommandChar command

applyCommandToBox :: Box -> Command -> Box
applyCommandToBox box (Command Remove label f) = frontLenses ++ otherLenses
  where
    frontLenses = takeWhile (notLabel label) box
    otherLenses = drop (length frontLenses + 1) box
    notLabel label lens = lenseLabel lens /= label
applyCommandToBox box (Command Insert label f)
  | isJust lenseToReplace = frontLenses ++ [Lens label f] ++ otherLenses
  | otherwise = box ++ [Lens label f]
  where
    hasLabel label lens = lenseLabel lens == label
    notLabel label lens = lenseLabel lens /= label
    lenseToReplace = find (hasLabel label) box
    frontLenses = takeWhile (notLabel label) box
    otherLenses = drop (length frontLenses + 1) box

applyCommand :: [Box] -> Command -> [Box]
applyCommand boxes (Command op label f) = frontBoxes ++ [newBox] ++ tail backBoxes
  where
    boxIndex = hashLabel label
    (frontBoxes, backBoxes) = splitAt boxIndex boxes
    newBox = applyCommandToBox (head backBoxes) (Command op label f)

applyCommands :: [Command] -> [Box] -> [Box]
applyCommands cs boxes = foldl applyCommand boxes cs

boxPower :: (Box,Int) -> Int
boxPower ([], _) = 0
boxPower (box, boxIndex) = sum $ map lensePower [0 .. length box - 1]
  where
    lensePower n = (boxIndex + 1) * (n + 1) * focal (box !! n)

totalPower :: [Box] -> Int
totalPower boxes = sum $ zipWith (curry boxPower) boxes [0..]

solve input = output
  where
    boxes = replicate 256 []
    commands = splitAtDelimiter (== ',') input
    parsedCommands = map parseCommand commands
    finalBoxes = applyCommands parsedCommands boxes
    output = totalPower finalBoxes