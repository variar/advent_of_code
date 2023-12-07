import Data.List
import Data.Maybe
import Data.Ord

type Hand = (String, Integer)

cards = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']

cardValue :: Char -> Integer
cardValue c = toInteger (fromJust (elemIndex c cards))

strength :: [Integer] -> Integer
strength [5] = 7
strength [1, 4] = 6
strength [2, 3] = 5
strength [1, 1, 3] = 4
strength [1, 2, 2] = 3
strength [1, 1, 1, 2] = 2
strength [1, 1, 1, 1, 1] = 1

handStrength :: Hand -> Integer
handStrength (hand, bid) = (strength . sort . filter (/= 0) . map countCards) cards
  where
    countCards c = (toInteger . length . filter (== c)) hand

compareByHighCard :: String -> String -> Ordering
compareByHighCard [] [] = EQ
compareByHighCard h1 h2
  | c1 < c2 = LT
  | c1 > c2 = GT
  | otherwise = compareByHighCard (tail h1) (tail h2)
  where
    c1 = cardValue (head h1)
    c2 = cardValue (head h2)

compareHands :: Hand -> Hand -> Ordering
compareHands h1 h2
  | s1 < s2 = LT
  | s1 > s2 = GT
  | otherwise = compareByHighCard (fst h1) (fst h2)
  where
    s1 = handStrength h1
    s2 = handStrength h2

parseHand :: String -> Hand
parseHand line = (head parts, stringToInt (last parts))
  where
    parts = words line
    stringToInt s = read s :: Integer

rankHands :: [Hand] -> [(Integer, Hand)]
rankHands hands = zip [1 ..] (sortBy compareHands hands)

handWin :: (Integer, Hand) -> Integer
handWin (rank, (hand, bid)) = rank * bid

printOutput input = show (output) ++ "\n"
  where
    hands = map parseHand input
    output = (sum . map handWin . rankHands) hands

main = interact (printOutput . lines)