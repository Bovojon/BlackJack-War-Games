import System.Random

data Number = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Read, Enum, Eq, Show, Ord)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Read, Enum, Eq, Show, Ord)

data PlayingCards = Card Number Suit deriving (Read, Eq, Show, Ord)

-- function to show card
showCard :: PlayingCards -> String
showCard (Card n s) = show n ++ " -- " ++ show s

-- function to take the top card
popCard :: Player -> PlayingCards
popCard player = head player

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

-------------------------------------------------------------------------------

dealTwoCards :: Deck -> (Player, Player, Deck)
dealTwoCards deck = (take 2 deck, take 2 (drop 2 deck), drop 4 deck)

dealOneCard :: Player -> Deck -> (Player, Deck)
dealOneCard player deck = (take 1 deck, drop 1 deck)

hit :: (Player, Deck) -> (Player, Deck)
hit (player, deck) = ((take 1 deck)++player, drop 1 deck)

getPlayerCards :: (Player, Player, Deck) -> Player
getPlayerCards (player, _, _) = player

getDealerCards :: (Player, Player, Deck) -> Player
getDealerCards (_, dealer, _) = dealer

getDealerFirstCard :: Player -> PlayingCards
getDealerFirstCard dealer = dealer !! 0

getDeck :: (Player, Player, Deck) -> Deck
getDeck (_, _, deck) = deck

doPlayerTurn :: (Player, Deck) -> (Player, Deck)
doPlayerTurn (player, deck) = do
  putStr "\nWhat would you like to do? (h/s): "
  action <- getLine

  if action == "h"
    then do
      -- In case of Hit
      let currentPlayerAndDeck = hit (player, deck)
      putStr "Your hand: "
      print (fst currentPlayerAndDeck)

      -- doPlayerTurn currentPlayerAndDeck

    else do
      putStrLn "\nYou chose to stand..."
      -- return currentPlayerAndDeck



main :: IO ()
main = do
  putStrLn "Welcome to BlackJack"
  putStrLn "---------------------------"
  putStrLn "\nShuffling cards..."
  let deck = shuffleDeck newDeck

  putStrLn "\nDealing cards...\n"
  let game_tuple = dealTwoCards deck

  putStr "Your cards: "
  let player = getPlayerCards game_tuple
  let deck = getDeck game_tuple
  print (player)

  putStr "Dealer's cards: "
  print (getDealerFirstCard(getDealerCards game_tuple), "Hidden Card")

  doPlayerTurn (player, deck)
