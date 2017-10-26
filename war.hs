import System.Random

data Number = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Read, Enum, Eq, Show, Ord)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Read, Enum, Eq, Show, Ord)

data PlayingCards = Card Number Suit deriving (Read, Eq, Show, Ord)

-- function to show card
showCard :: PlayingCards -> String
showCard (Card n s) = show n ++ " -- " ++ show s

-- function to check if the cards are same
isSameCard :: PlayingCards -> PlayingCards -> Bool
isSameCard card1 card2
    | card1 == card2  = True
    | otherwise       = False

-- function to check the order of the cards
isGreaterCard :: PlayingCards -> PlayingCards -> Bool
isGreaterCard card1 card2
    | card1 > card2  = True
    | otherwise      = False
--------------------------------------------------------------------------------------------------------

type Deck = [PlayingCards]
type Player = [PlayingCards]

newDeck :: Deck
newDeck = [Card x y|  y <- [Clubs .. Spades],x <- [Two .. Ace]]

-- shuffle two decks
shuffle :: Deck -> Deck -> Deck
shuffle [] [] = []
shuffle (c1:d1) (c2:d2) = [c1,c2] ++ shuffle d1 d2

splitShuffle :: Deck -> Deck
splitShuffle deck = shuffle deck1 deck2
                 where n = (length deck) `div` 2
                       deck1 = fst (splitAt n deck)
                       deck2 = snd (splitAt n deck)

shuffleDeck :: Deck -> Deck
shuffleDeck deck = do
    -- n <- randomRIO(3,8) :: IO Int
    d <- (iterate splitShuffle deck) !! (4)
    return d

dealCards :: Deck -> (Player, Player)
dealCards deck = ([deck !! n | n <- [0,2 ..51]], [deck !! n | n <- [1,3 ..51]])

--------------------------------------------------------------------------------------------------------

winner :: Deck -> Deck -> Deck
winner cardsA cardsB
  | length cardsA == 0 = cardsB
  | length cardsB == 0 = cardsA

pop :: [a] -> [a]
pop ((:) x xs) = xs

insert :: a -> [a] -> [a]
insert a list_a = list_a ++ [a]

removeThreeAdd :: Deck -> Deck
removeThreeAdd cards = [cards !! n | n <- [3..(length cards)]] ++ [cards !! n | n <- [0..3]]

goToWar :: Deck -> Deck -> Deck
goToWar cardsA cardsB
  | (||) (length cardsA == 0) (length cardsB == 0) = winner cardsA cardsB
  | (head cardsA) `isGreaterCard` (head cardsB) = goToWar (insert (head cardsA) cardsB) (pop cardsA)
  | (head cardsA) `isSameCard` (head cardsB) = goToWar (removeThreeAdd cardsA) (removeThreeAdd cardsB)
