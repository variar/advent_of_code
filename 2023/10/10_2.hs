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

rayToNode :: Int -> Int -> [Int]
rayToNode lineWidth n = [line * lineWidth .. n]
  where
    (line, posInLine) = divMod n lineWidth

type PipeAtFn = Int -> Char

isIntersectionAt :: PipeAtFn -> Int -> Bool
isIntersectionAt pipeAtFn index
  | pipe == '|' = True
  | pipe == 'J' = True
  | pipe == 'L' = True
  | otherwise = False
  where
    pipe = pipeAtFn index

nodesInPath :: PipeAtFn -> Int -> Int -> [Int]
nodesInPath pipeAtFn lineWidth n = filter (isIntersectionAt pipeAtFn) nodes
  where
    nodes = rayToNode lineWidth n

countIntersections :: PipeAtFn -> Int -> Int -> Int
countIntersections pipeAtFn lineWidth n = length $ nodesInPath pipeAtFn lineWidth n

isInsideLoop :: PipeAtFn -> Int -> Int -> Bool
isInsideLoop pipeAtFn lineWidth n = odd (countIntersections pipeAtFn lineWidth n)

notMainLoop :: [Node] -> Int -> Bool
notMainLoop mainLoopNodes n = not $ any ((== n) . fst) mainLoopNodes

startPipe :: PipesMap -> Node -> Node -> Node -> Char
startPipe pipesMap start loopStart loopEnd
  | c1 == i - 1 && c2 == i + 1 = '-'
  | c1 == i - 1 && c2 == i + w = '7'
  | c1 == i - w && c2 == i - 1 = 'J'
  | c1 == i - w && c2 == i + 1 = 'L'
  | c1 == i - w && c2 == i + w = '|'
  | c1 == i + 1 && c2 == i + w = 'F'
  where
    i = fst start
    connections = sort [fst loopStart, fst loopEnd]
    (c1, c2) = (head connections, last connections)
    w = snd pipesMap

pipeAt :: PipesMap -> Char -> [Int] -> Int -> Char
pipeAt pipesMap sPipe notMainLoop index
  | pipe == 'S' = sPipe
  | index `elem` notMainLoop = '.'
  | otherwise = pipe
  where
    pipe = fst pipesMap ! index

cleanupPipesMap :: PipesMap -> Char -> [Int] -> PipesMap
cleanupPipesMap pipesMap sPipe notMainLoop = (cleanMap, snd pipesMap)
  where
    cleanMapList = map (pipeAt pipesMap sPipe notMainLoop) [0 .. length (fst pipesMap) - 1]
    cleanMap = listArray (0, length cleanMapList - 1) cleanMapList

pipeAtCleanMap :: PipesMap -> Int -> Char
pipeAtCleanMap cleanPipesMap index = fst cleanPipesMap ! index

solve input = length output
  where
    (pipesMap, start) = parsePipes input
    graph = buildGraph pipesMap
    (loopStart, loopEnd) = mainLoop graph start
    mainLoopNodes = graph ! start : traceMainLoop graph (graph ! loopEnd) (graph ! start) (graph ! loopStart)
    mainLoopIndexes = sort (map fst mainLoopNodes)
    notMainLoopNodes = [0 .. length graph - 1] \\ mainLoopIndexes
    sPipe = startPipe pipesMap (graph ! start) (graph ! loopStart) (graph ! loopEnd)
    cleanPipesMap = cleanupPipesMap pipesMap sPipe notMainLoopNodes
    pipeAtFn = pipeAtCleanMap cleanPipesMap
    output = filter (isInsideLoop pipeAtFn (snd pipesMap)) notMainLoopNodes
