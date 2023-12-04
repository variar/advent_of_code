splitAtDelimiter :: (Char -> Bool) -> String -> [String]
splitAtDelimiter p s = case dropWhile p s of
  "" -> []
  s' -> w : splitAtDelimiter p s''
    where
      (w, s'') = break p s'

stringToInt s = read s :: Integer

splitToNumbers :: String -> [Integer]
splitToNumbers s = map stringToInt (splitAtDelimiter (== ' ') s)

type Card = (Integer, Integer, [Integer], [Integer])

parseCard :: String -> Card
parseCard s = (cardNumber, 1, splitToNumbers leftSide, splitToNumbers rightSide)
  where
    cardParts = splitAtDelimiter (== ':') s
    cardNumber = stringToInt (last (splitAtDelimiter (== ' ') (head cardParts)))
    sides = splitAtDelimiter (== '|') (last cardParts)
    (leftSide, rightSide) = (head sides, last sides)

setCardsCount :: Integer -> [Card] -> [Card]
setCardsCount n [] = []
setCardsCount n cards = newCard : setCardsCount n (tail cards)
  where
    (index, oldCount, numbers, winning) = head cards
    newCard = (index, n, numbers, winning)

isWinningNumber :: [Integer] -> Integer -> Integer
isWinningNumber winning n = if n `elem` winning then 1 else 0

countWinningNumbers :: [Integer] -> [Integer] -> Integer
countWinningNumbers numbers winning = sum (map (isWinningNumber winning) numbers)

isSameCard :: Card -> Card -> Bool
isSameCard (lhs, _, _, _) (rhs, _, _, _) = lhs == rhs

notSameCard :: Card -> Card -> Bool
notSameCard c1 c2 = not (isSameCard c1 c2)

addCard :: [Card] -> Card -> [Card]
addCard cards card = prefix ++ [newCard] ++ suffix
  where
    prefix = takeWhile (notSameCard card) cards
    sameCards = dropWhile (notSameCard card) cards
    (_, count, _, _) = card
    (index, oldCount, numbers, winning) = head sameCards
    newCard = (index, oldCount + count, numbers, winning)
    suffix = dropWhile (isSameCard card) sameCards

addCards :: [Card] -> [Card] -> [Card]
addCards cards [] = cards
addCards cards newCards = addCards (addCard cards (head newCards)) (tail newCards)

winMoreCards :: [Card] -> Card -> [Card]
winMoreCards originalCards card = setCardsCount count newCards
  where
    (cardIndex, count, numbers, winning) = card
    winningCards = countWinningNumbers numbers winning
    newCardIndices = if winningCards > 0 then [fromInteger cardIndex .. fromIntegral cardIndex + fromIntegral winningCards - 1] else []
    newCards = map (originalCards !!) newCardIndices

collectCards :: [Card] -> [Card] -> Int
collectCards originalCards cards = points + (if not (null newCards) then collectCards originalCards newCards else 0)
  where
    nextCard = head cards
    (_, count, _, _) = nextCard
    cardsToAdd = winMoreCards originalCards nextCard
    points = fromInteger count * length cardsToAdd
    newCards = tail (addCards cards cardsToAdd)

totalScore :: [String] -> Int
totalScore cards = collectCards allCards allCards + length allCards
  where
    allCards = map parseCard cards

printScore :: [String] -> [Char]
printScore s = show (totalScore s) ++ "\n"

collectCardsDebug :: [Card] -> [Card] -> (Int, [Card])
collectCardsDebug allCards cards = (points, newCards)
  where
    cardsToAdd = winMoreCards allCards (head cards)
    points = length cardsToAdd
    newCards = tail (addCards cards cardsToAdd)

printDebug s = show (p3, cardsAfterStep3) ++ "\n"
  where
    allCards = map parseCard s
    (p1, cardsAfterStep1) = collectCardsDebug allCards allCards
    (p2, cardsAfterStep2) = collectCardsDebug allCards cardsAfterStep1
    (p3, cardsAfterStep3) = collectCardsDebug allCards cardsAfterStep2

main = interact (printScore . lines)
