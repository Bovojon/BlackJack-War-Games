import System.Random
import System.Exit (exitSuccess)
import Data.List

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
shuffleDeck :: Deck -> IO Deck
shuffleDeck deck = do
    if length deck /= 0
        then do 
            let deckLen = (length deck) - 1
            n <- randomRIO(0, deckLen) :: IO Int
            let randomCard = deck !! (fromIntegral n)
            tailShuffle <- shuffleDeck (delete randomCard deck)
            return ([randomCard] ++ tailShuffle)
        else return deck
{-        
shuffle :: Deck -> Deck -> Deck
shuffle [] [] = []
shuffle (c1:d1) (c2:d2) = [c1,c2] ++ shuffle d1 d2

-- Method to split the deck into equal two parts and shuffle it
splitShuffle :: Deck -> Deck
splitShuffle deck = shuffle deck1 deck2
                 where n = (length deck) `div` 2
                       deck1 = fst (splitAt n deck)
                       deck2 = snd (splitAt n deck)

shuffleDeck :: Deck -> IO Deck
shuffleDeck deck = do
    do
    n <- randomRIO(3,8) :: IO Int
    return ((iterate splitShuffle deck) !! (fromIntegral n))-}

-------------------------------------------------------------------------------

dealTwoCards :: Monad m => m Deck -> m (Player, Player, Deck)
dealTwoCards d = 
    do
    deck <- d
    return (take 2 deck, take 2 (drop 2 deck), drop 4 deck)

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
      
      putStr "Your Score: "
      if (dealerOver21 player')
          then print (lowScore player')
          else print (highScore player')
      
      if (scoreOver21 player')
          then do 
              putStrLn "\n****************************"
              putStr "You went over 21. You lose!\n"
              exitSuccess
          else do
              gameStatus <- (doPlayerTurn (player', deck'))
              return gameStatus

    else if action == "s"
      then  do
          putStrLn "\n--------------------------------"
          putStrLn "You chose to stand..."
          putStrLn "--------------------------------"
          return (player, deck)
      else do
          putStrLn "*** Invalid Move. Press either h or s. (hit/stand)"
          gameStatus <- (doPlayerTurn (player, deck))
          return gameStatus
      
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
highScore player = sum $ map cardValue2 player

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
    
    
    putStr "\nDealer's cards: "
    print dealer
    putStr "Dealer's score: "
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
  
blackJack :: Player -> Bool
blackJack player = (highScore player) == 21

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
  let gameTuple = dealTwoCards deck
  
  game_tuple <- gameTuple
  putStrLn "--------------------------------"
  
  putStr "Your cards: "
  let player = getPlayerCards game_tuple
  let deck = getDeck game_tuple
  print (player)
  putStr "Your score: "
  print (highScore player)

  putStr "Dealer's cards: "
  let dealer = getDealerCards game_tuple
  print (getDealerFirstCard(dealer), "Hidden Card")
  putStr "Dealer's score is greater than: "
  print (cardValue2 (dealer!!0))
  
  if (blackJack player)
      then do
          putStrLn "\n**************************"
          if (blackJack dealer)
          then putStrLn "Both player have BlackJack. The game is a tie."
          else putStrLn "BlackJack! You won the game."
      else doTurns (player, dealer, deck)
  putStrLn "\n************************"
  putStrLn "End of Game \n"
