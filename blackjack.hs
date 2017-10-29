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
   d <- (iterate splitShuffle deck) !! (3)
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
              putStrLn "\n****************************"
              putStr "You went over 21. You lose!\n"
              exitSuccess
          else do
              gameStatus <- (doPlayerTurn (player', deck'))
              return gameStatus

    else do
      putStrLn "\n--------------------------------"
      putStrLn "You chose to stand..."
      putStrLn "--------------------------------"
      return (player, deck)
      
doDealerTurn :: (Player, Deck) -> IO (Player, Deck)
doDealerTurn (dealer, deck) = do
    
    if (highScore dealer < 17)
    then do
      putStrLn "\nDealer hits..."
      let (dealer', deck') = hit (dealer, deck)
      
      putStr "Dealer's hand: "
      print dealer'
      putStr "Dealer's Score: "
      print (highScore dealer')
      
      if (dealerOver21 dealer')
          then do 
              putStrLn "\n****************************"
              putStr "Dealer went over 21. You Win!\n"
              exitSuccess
          else do
              gameStatus <- (doDealerTurn (dealer', deck'))
              return gameStatus
      
    else return (dealer, deck)

-- Card Value when Ace ==1
cardValue :: PlayingCards -> Int
cardValue (Card Ace _) = 1
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

-- Card Value when Ace ==1
cardValue2 :: PlayingCards -> Int
cardValue2 (Card Ace _) = 11
cardValue2 (Card King _) = 10
cardValue2 (Card Queen _) = 10
cardValue2 (Card Jack _) = 10
cardValue2 (Card Two _) = 2
cardValue2 (Card Three _) = 3
cardValue2 (Card Four _) = 4
cardValue2 (Card Five _) = 5
cardValue2 (Card Six _) = 6
cardValue2 (Card Seven _) = 7
cardValue2 (Card Eight _) = 8
cardValue2 (Card Nine _) = 9
cardValue2 (Card Ten _) = 10

lowScore :: Player -> Int
lowScore player = sum $ map cardValue player

highScore :: Player -> Int
highScore player = sum $ map cardValue player

scoreOver21 :: Player -> Bool
scoreOver21 player =
  lowScore player > 21

dealerOver21 :: Player -> Bool
dealerOver21 player =
  highScore player > 21
  
declareWinner:: Player -> Player -> IO()
declareWinner player dealer = do
    putStr "\nYour cards: "
    print player
    putStr "Your score: "
    print (highScore player)
    
    
    putStr "\nDealer cards: "
    print dealer
    putStr "Your score: "
    print (highScore dealer)
    
    putStrLn "\n**************************"
    if (highScore player) > (highScore dealer)
        then putStrLn "Congrats! You Win"
        else if (highScore player) == (highScore dealer)
            then putStrLn "The game is a tie."
            else putStrLn "You Lose!"

doTurns:: (Player, Player, Deck) -> IO()
doTurns (player, dealer, deck) = do
    ------------------------------------------------------------------------------------
  -- Player turn
  ------------------------------------------------------------------------------------
  putStrLn "\n--------------------------------"
  putStrLn "Your Turn..."
  putStrLn "--------------------------------"
  (player', deck') <- doPlayerTurn (player, deck)
  
  ------------------------------------------------------------------------------------
  -- Dealer turn
  ------------------------------------------------------------------------------------
  putStrLn "\n--------------------------------"
  putStrLn "Dealer's Turn..."
  putStrLn "--------------------------------\n"
  putStr "Your cards: "
  print player'
  
  putStr "Dealer's hand: "
  print dealer
  (dealer', deck'') <- doDealerTurn (dealer, deck')
  putStrLn "\n--------------------------------"
  putStrLn "Dealer Stands... "
  putStrLn "--------------------------------"
  ------------------------------------------------------------------------------------
  -- Dealer turn Winner
  ------------------------------------------------------------------------------------
  
  declareWinner player' dealer'
  
  putStrLn "\n************************"
  putStrLn "End of Game \n"

main :: IO ()
main = do
  putStrLn "Welcome to BlackJack"
  putStrLn "---------------------------"
  
  ------------------------------------------------------------------------------------
  -- Shuffle, deal and show Cards
  ------------------------------------------------------------------------------------
  putStrLn "\nShuffling cards..."
  let deck = shuffleDeck newDeck

  putStrLn "\nDealing cards...\n"
  let game_tuple = dealTwoCards deck

  putStr "Your cards: "
  let player = getPlayerCards game_tuple
  let deck = getDeck game_tuple
  print (player)

  putStr "Dealer's cards: "
  putStrLn "\n--------------------------------"
  let dealer = getDealerCards game_tuple
  print (getDealerFirstCard(dealer), "Hidden Card")
  
  doTurns (player, dealer, deck)
