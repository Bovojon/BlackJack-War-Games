import System.Random
import System.Exit (exitSuccess)
import Data.List

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

shuffle :: Deck -> IO Deck
shuffle deck = do
    if length deck /= 0
        then do 
            let deckLen = (length deck) - 1
            n <- randomRIO(0, deckLen) :: IO Int
            let randomCard = deck !! (fromIntegral n)
            tailShuffle <- shuffle (delete randomCard deck)
            return ([randomCard] ++ tailShuffle)
        else return deck

dealCards :: Monad m => m Deck -> m (Player, Player)
dealCards d = 
    do
    deck <- d
    return ([deck !! n | n <- [0,2 ..51]], [deck !! n | n <- [1,3 ..51]])

skipThreeCards :: Player -> Player
skipThreeCards cards = (drop 4 cards) ++ (take 4 cards)

roundWinner :: Player -> Player -> IO (Player, Player)
roundWinner cardsA cardsB
  | (head cardsA) `isGreaterCard` (head cardsB) =
    do
      putStr "\nPlayerA: "
      print (head cardsA)
      putStr "PlayerB: "
      print (head cardsB)
      putStrLn "-> PlayerA won the round!\n"
      return (((tail cardsA) ++ [(head cardsB)] ++ [(head cardsA)]), (tail cardsB))
  | (head cardsB) `isGreaterCard` (head cardsA) =
    do
      putStr "\nPlayerA: "
      print (head cardsA)
      putStr "PlayerB: "
      print (head cardsB)
      putStrLn "-> PlayerB won the round!\n"
      return ((tail cardsA), ((tail cardsB) ++ [(head cardsA)] ++ [(head cardsB)]))
  
  | (head cardsA) `isSameCard` (head cardsB) =
    do
      if  length cardsA < 4
          then do 
              putStrLn "You do no have enough cards to continue. You lose!"
              exitSuccess
          
          else if length cardsB < 4
              then do 
                  putStrLn "You win!"
                  exitSuccess
              else do 
                  putStrLn "Both of you have same the same card. Go for another round."
                  return ((skipThreeCards cardsA), (skipThreeCards cardsB))
      
     
--autowars :: (Player, Player) -> String
autowars (player1, player2)
    | (length player1) == 0 = putStrLn "Player 1 lost the game"
    | (length player2) == 0 = putStrLn "Player 2 lost the game"
    | otherwise = do
                    game <- roundWinner player1 player2
                    autowars game


--interactiveWar :: (Player, Player) -> String
interactiveWar (player1, player2)
    | (length player1) == 0 = putStrLn "Player 1 lost the game"
    | (length player2) == 0 = putStrLn "Player 2 lost the game"
    | otherwise = do
                    game <- roundWinner player1 player2
                    putStrLn "Press enter for next round"
                    input <- getLine
                    interactiveWar game          
main :: IO ()
main = do
  putStrLn "Welcome to Wars"
  putStrLn "---------------------------"
  
  ------------------------------------------------------------------------------------
  -- Shuffle, deal and show Cards
  ------------------------------------------------------------------------------------
  putStrLn "\nShuffling and Dealing cards..."
  game <- dealCards (shuffle newDeck)
  
  putStrLn "--------------------------------"
  
  autowars game
  
  interactiveWar game

  putStrLn "\n************************"
  putStrLn "End of Game \n"
