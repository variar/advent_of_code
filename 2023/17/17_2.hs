import Data.Array (Array)
import Data.Array qualified as Array
import Data.List (minimumBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow, traceShowId)
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

data Direction = N | E | W | S deriving (Eq, Show, Ord)

data DStep = DStep {stepDirection :: Direction, stepsCount :: Int, stepX :: Int, stepY :: Int} deriving (Show, Eq, Ord)

data Graph = Graph {width :: Int, height :: Int, heat :: Array Int Int} deriving (Show)

flipDirection :: Direction -> Direction
flipDirection S = N
flipDirection N = S
flipDirection E = W
flipDirection W = E

addMovesInSameDirection :: DStep -> DStep -> DStep
addMovesInSameDirection (DStep addD addSteps _ _) (DStep curD curSteps curX curY)
  | addD == curD = DStep curD (curSteps + addSteps) curX curY
  | otherwise = DStep curD curSteps curX curY

neighbors :: Graph -> DStep -> [DStep]
neighbors (Graph w h _) cur = allowedMoves
  where
    (DStep curD curCount curX curY) = cur

    isInside step = stepX step >= 0 && stepX step < w && stepY step >= 0 && stepY step < h
    movesInsideGraph = filter isInside [DStep N 1 curX (curY - 1), DStep S 1 curX (curY + 1), DStep E 1 (curX + 1) curY, DStep W 1 (curX - 1) curY]

    isForward step = stepDirection step /= flipDirection curD
    movesForward = filter isForward movesInsideGraph

    canTurn (DStep d _ _ _)
      | d /= curD = curCount >= 4
      | otherwise = True

    pruneTurns = filter canTurn movesForward

    ifNotTooStraight (DStep _ c _ _) = c <= 10
    allowedMoves = filter ifNotTooStraight $ map (addMovesInSameDirection cur) pruneTurns

heatLossCost :: Graph -> DStep -> Int
heatLossCost (Graph w _ heat) (DStep _ _ curX curY) = heat Array.! (curX + curY * w)

heuristics :: Graph -> DStep -> Int
heuristics (Graph w h _) (DStep _ _ x y) = w - x + h - y

data SearchState = SearchState {toVisit :: Set (Int, DStep), visited :: Set DStep, tentativeHeatLoss :: Map DStep Int} deriving (Show)

selectNextToVisit :: Graph -> SearchState -> DStep
selectNextToVisit g (SearchState toVisit visited _) = snd (Set.elemAt 0 toVisit)

recordHeatLoss :: Graph -> (DStep, Int) -> SearchState -> SearchState
recordHeatLoss g (pos, loss) (SearchState toVisit visited heatLossMap) = SearchState nextToVisit visited nextTentativeHeatLoss
  where
    currentLoss = Map.lookup pos heatLossMap
    h = heuristics g pos
    updatedLoss
      | isJust currentLoss = min (fromJust currentLoss) loss
      | otherwise = loss

    currentToVisitRecord
      | isJust currentLoss = (fromJust currentLoss + h, pos)
      | otherwise = (h, pos)
    updatedToVisitRecord = (updatedLoss + h, pos)

    nextToVisit = Set.insert updatedToVisitRecord . Set.delete currentToVisitRecord $ toVisit
    nextTentativeHeatLoss = Map.insert pos updatedLoss heatLossMap

recordVisited :: Graph -> DStep -> SearchState -> SearchState
recordVisited g pos (SearchState toVisit visited heatLossMap) = SearchState nextToVisit nextVisited heatLossMap
  where
    (DStep nowD nowS nowX nowY) = pos
    makeVisitedStep k = DStep nowD k nowX nowY

    allVisitedStates = if nowS < 4 then [pos] else map makeVisitedStep [nowS .. 10]
    nextVisited = foldr Set.insert visited allVisitedStates

    currentHeatLossCost = fromJust (Map.lookup pos heatLossMap)
    h = heuristics g pos
    nextToVisit = foldr (Set.delete . (currentHeatLossCost + h,)) toVisit allVisitedStates

visit :: Graph -> DStep -> SearchState -> SearchState
visit g target searchState
  | stepX currentNode == stepX target && stepY currentNode == stepY target = nextState
  | otherwise = visit g target nextState
  where
    (SearchState toVisit visited heatLossMap) = searchState
    currentNode = traceShow (length toVisit, length visited, length heatLossMap) selectNextToVisit g searchState
    currentHeatLossCost = fromJust (Map.lookup currentNode heatLossMap)

    canVisit n = Set.notMember n visited
    nextNodes = filter canVisit $ neighbors g currentNode

    nextNodesHeatLoss = zip nextNodes (map ((+ currentHeatLossCost) . heatLossCost g) nextNodes)
    updatedState = foldr (recordHeatLoss g) searchState nextNodesHeatLoss

    nextState = recordVisited g currentNode updatedState

minHeat :: Graph -> Int
minHeat g = minimum (map snd heatLosses)
  where
    (Graph w h _) = g

    toVisit = (Set.insert (w + h, DStep E 0 0 0) . Set.insert (w + h, DStep S 0 0 0)) Set.empty
    initialHeatLoss = (Map.insert (DStep E 0 0 0) 0 . Map.insert (DStep S 0 0 0) 0) Map.empty
    initialState = SearchState toVisit Set.empty initialHeatLoss
    target = DStep E 0 (w - 1) (h - 1)

    finalState = visit g target initialState

    isTarget (n, _) = (stepsCount n >= 4) && stepX n == stepX target && stepY n == stepY target
    heatLosses = filter isTarget (Map.toList (tentativeHeatLoss finalState))

parseGraph :: String -> Graph
parseGraph input = Graph w h (Array.listArray (0, length heatLosses) heatLosses)
  where
    ls = lines input
    w = length (head ls)
    h = length ls
    readInt c = read [c] :: Int
    heatLosses = map readInt (filter (/= '\n') input)

solve input = output
  where
    g = parseGraph input
    output = minHeat g