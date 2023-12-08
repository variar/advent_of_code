import Data.Array
import Data.List
import Data.Maybe
import Debug.Trace
import System.Environment
import System.FilePath

splitAtDelimiter :: (Char -> Bool) -> String -> [String]
splitAtDelimiter p s = case dropWhile p s of
  "" -> []
  s' -> w : splitAtDelimiter p s''
    where
      (w, s'') = break p s'

type Node = (String, Int, Int)

type NodeArray = Array Int Node

parseMapEntry :: [String] -> String -> Node
parseMapEntry mapNodes line = ((head . words) line, fromJust (elemIndex left mapNodes), fromJust (elemIndex right mapNodes))
  where
    directionsPart = take 8 (last (splitAtDelimiter (== '(') line))
    directions = splitAtDelimiter (== ',') directionsPart
    (left, right) = (head directions, drop 1 (last directions))

isStartNode :: Node -> Bool
isStartNode node
  | last n == 'A' = True
  | otherwise = False
  where
    (n, l, r) = node

isEndNode :: Node -> Bool
isEndNode node
  | last n == 'Z' = True
  | otherwise = False
  where
    (n, l, r) = node

isEnd :: [Node] -> Bool
isEnd = all isEndNode

collectStartNodes :: [Node] -> [Node]
collectStartNodes = filter isStartNode

step :: NodeArray -> Char -> Node -> Node
step mapNodesArray d node
  | d == 'L' = mapNodesArray ! l
  | d == 'R' = mapNodesArray ! r
  where
    (n, l, r) = node

type StepFn = Char -> Node -> Node

doOneStep :: StepFn -> Char -> Integer -> Node -> (Node, Integer)
doOneStep stepFn direction stepsCounter node = if isEndNode newNode then traceShowId result else result
  where
    newNode = stepFn direction node
    result = (newNode, stepsCounter + 1)

stepsForNode :: StepFn -> String -> Integer -> Node -> Integer
stepsForNode stepFn directions stepsCounter node
  | isEndNode node = stepsCounter
  | otherwise = stepsForNode stepFn (tail directions) stepNum newNode
  where
    (newNode, stepNum) = doOneStep stepFn (head directions) stepsCounter node

solve input = output
  where
    instructions = cycle (head input)
    mapNodes = map (head . words) (drop 2 input)
    mapEntries = map (parseMapEntry mapNodes) (drop 2 input)
    mapEntriesArray = listArray (0, length mapEntries) mapEntries
    stepFn = step mapEntriesArray
    startNodes = collectStartNodes mapEntries
    cyclesPerNode = map (stepsForNode stepFn instructions 0) startNodes
    output = foldr lcm 1 cyclesPerNode

main = do
  name <- getProgName
  args <- getArgs
  let inputFile =
        if not (null args)
          then head args
          else take 2 (dropExtension name) ++ "_input"

  input <- readFile inputFile
  (print . solve . lines) input