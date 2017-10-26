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

pop :: [a] -> [a]  -- return a tuple containing the popped element and the new stack
pop [] = error "Can't pop from an empty stack!"
pop ((:) x stack) = stack

insert :: a -> [a] -> [a]
insert a list_a = list_a ++ [a]
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

compare :: Player -> Player -> Player
compare playerA playerB =
  if (head playerA) `isGreaterCard` (head playerB)
    then insert (head playerA) playerB and pop playerA
    else if (head playerA) `isSameCard` (head playerB) then
