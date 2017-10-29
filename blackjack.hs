import System.Random
import System.Exit (exitSuccess)

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

doPlayerTurn :: (Player, Deck) -> IO (Player, Deck)
doPlayerTurn (player, deck) = do
  putStr "\nWhat would you like to do? (h/s): "
  action <- getLine
  
  if action == "h"
    then do
      -- In case of Hit
      let (player', deck') = hit (player, deck)
      putStr "Your hand: "
      print player'
      
      if (scoreOver21 player')
          then do 
              putStr "You went over 21. You lose!\n"
              exitSuccess
          else do
              output <- (doPlayerTurn (player', deck'))
              return output

    else do
      putStrLn "\nYou chose to stand..."
      return (player, deck)
      


cardValue :: PlayingCards -> Int
cardValue (Card Ace _) = 11
cardValue (Card King _) = 10
cardValue (Card Queen _) = 10
cardValue (Card Jack _) = 10
cardValue (Card Two _) = 2
cardValue (Card Three _) = 3
cardValue (Card Four _) = 4
cardValue (Card Five _) = 5
cardValue (Card Six _) = 6
cardValue (Card Seven _) = 7
cardValue (Card Eight _) = 8
cardValue (Card Nine _) = 9
cardValue (Card Ten _) = 10

handScore :: Player -> Int
handScore player = sum $ map cardValue player

scoreOver21 :: Player -> Bool
scoreOver21 player =
  handScore player > 21
  
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

  ------------------------------------------------------------------------------------
  -- Player turn
  ------------------------------------------------------------------------------------
  (player', deck') <- doPlayerTurn (player, deck)
  
  ------------------------------------------------------------------------------------
  -- Dealer turn
  ------------------------------------------------------------------------------------
  
  
  
  putStr "End "
  

