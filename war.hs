import System.Random

data Number = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Read, Enum, Eq, Show, Ord)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Read, Enum, Eq, Show, Ord)

data PlayingCards = Card Number Suit deriving (Read, Eq, Show, Ord)

data Roundwinner = Player1 | Player2 | Tie

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

-- function to take the top card
popCard :: Player -> PlayingCards
popCard player = head player
--------------------------------------------------------------------------------------------------------

type Deck = [PlayingCards]
type Player = [PlayingCards]

newDeck :: Deck
newDeck = [Card x y|  y <- [Clubs .. Spades], x <- [Two .. Ace]]

-- shuffle two decks
shuffle :: Deck -> Deck -> Deck
shuffle [] [] = []
shuffle (c1:d1) (c2:d2) = [c1,c2] ++ shuffle d1 d2

-- Method to split the deck into equal two parts and shuffle it
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

goToWar :: Deck -> Deck -> Deck
goToWar cardsA cardsB
  | (||) (length cardsA == 0) (length cardsB == 0) = winner cardsA cardsB
  | (head cardsA) `isGreaterCard` (head cardsB) = goToWar (insert (head cardsB) cardsA) (pop cardsB)
  | (head cardsA) `isSameCard` (head cardsB) = goToWar (removeThreeAdd cardsA) (removeThreeAdd cardsB)

roundWinner cardsA cardsB
  | (head cardsA) `isGreaterCard` (head cardsB) =
    do
      putStr "PlayerA: "
      print (head cardsA)
      putStr "PlayerB: "
      print (head cardsB)
      putStrLn "PlayerA won the round"
      return (((tail cardsA) ++ [(head cardsB)] ++ [(head cardsA)]), (tail cardsB))
  | (head cardsB) `isGreaterCard` (head cardsA) =
    do
      putStr "PlayerA: "
      print (head cardsA)
      putStr "PlayerB: "
      print (head cardsB)
      putStrLn "PlayerB won the round"
      return ((tail cardsA), ((tail cardsB) ++ [(head cardsA)] ++ [(head cardsB)]))

  -- |(head cardsA) `isSameCard` (head cardsB) =


-- autoWar :: IO ()
-- autoWar =
--   do
--     let cards = shuffleDeck newDeck
--     let players_tuple = dealCards cards
--     let player1 = fst players_tuple
--     let player2 = snd players_tuple
--     roundWinner
