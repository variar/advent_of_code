import Data.Array
import Data.List
import Data.Maybe (fromJust)
import System.Environment
import System.FilePath (dropExtension)

main = do
  name <- getProgName
  args <- getArgs
  let inputFile =
        if not (null args)
          then head args
          else take 2 (dropExtension name) ++ "_input"

  input <- readFile inputFile
  print . solve . lines $ input

type PipesMap = (Array Int Char, Int)

parsePipes :: [String] -> (PipesMap, Int)
parsePipes lines = ((pipesMap, width), start)
  where
    width = length (head lines) + 2
    addDots l = ['.'] ++ l ++ ['.']
    dotLines = map addDots lines
    dotLine = replicate width '.'
    pipesMapList = dotLine ++ concat dotLines ++ dotLine
    start = fromJust $ elemIndex 'S' pipesMapList
    pipesMap = listArray (0, length pipesMapList - 1) pipesMapList

type Node = (Int, [Int])

type PipesGraph = Array Int Node

neighbors :: PipesMap -> Int -> Node
neighbors pipesMap index
  | pipe == '|' = (index, [index + w, index - w])
  | pipe == '-' = (index, [index + 1, index - 1])
  | pipe == '7' = (index, [index - 1, index + w])
  | pipe == 'L' = (index, [index + 1, index - w])
  | pipe == 'F' = (index, [index + 1, index + w])
  | pipe == 'J' = (index, [index - 1, index - w])
  | otherwise = (index, [])
  where
    pipe = fst pipesMap ! index
    w = snd pipesMap

buildGraph :: PipesMap -> PipesGraph
buildGraph pipesMap = listArray (0, length graph - 1) graph
  where
    graph = map (neighbors pipesMap) [0 .. length (fst pipesMap) - 1]

mainLoop :: PipesGraph -> Int -> (Int, Int)
mainLoop graph start = (fst $ head loopEnds, fst $ last loopEnds)
  where
    isNextTo node = start `elem` snd node
    loopEnds = filter (isNextTo . snd) (assocs graph)

makeStep :: PipesGraph -> Node -> Node -> Node
makeStep graph prevNode currentNode = graph ! nextIndex
  where
    nextIndex = head $ filter (/= fst prevNode) (snd currentNode)

traceMainLoop :: PipesGraph -> Node -> Node -> Node -> [Node]
traceMainLoop graph loopEnd prevNode currentNode
  | fst currentNode == fst loopEnd = [currentNode]
  | otherwise = currentNode : traceMainLoop graph loopEnd currentNode nextNode
  where
    nextNode = makeStep graph prevNode currentNode

solve input = output
  where
    (pipesMap, start) = parsePipes input
    graph = buildGraph pipesMap
    (loopStart, loopEnd) = mainLoop graph start
    mainLoopNodes = traceMainLoop graph (graph ! loopEnd) (graph ! start) (graph ! loopStart)
    output = (length mainLoopNodes + 1) `div` 2