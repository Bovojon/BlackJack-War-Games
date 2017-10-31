import System.Random
import System.Exit (exitSuccess)
import Data.List

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
      
     
autowars :: (Player, Player) -> IO ()
autowars (playerA, playerB)
    | (length playerA) == 0 = putStrLn "\n*** Player A lost the game ***\n"
    | (length playerB) == 0 = putStrLn "\n*** Player B lost the game ***\n"
    | otherwise = do
                    game <- roundWinner playerA playerB
                    putStr "Score Player A: "
                    print (length playerA)
                    
                    putStr "Score Player B: "
                    print (length playerB)
                    autowars game


interactiveWar :: (Player, Player) -> IO ()
interactiveWar (playerA, playerB)
    | (length playerA) == 0 = putStrLn "\n*** Player A lost the game ***\n"
    | (length playerB) == 0 = putStrLn "\n*** Player B lost the game ***\n"
    | otherwise = do
                    game <- roundWinner playerA playerB
                    putStr "Score Player A: "
                    print (length playerA)
                    
                    putStr "Score Player B: "
                    print (length playerB)
                    
                    putStrLn "Press enter for next round -> "
                    input <- getLine
                    putStr ""
                    
                    interactiveWar game          
main :: IO ()
main = do
  putStrLn "\nWelcome to Wars Game"
  putStrLn "---------------------------"
  
  ------------------------------------------------------------------------------------
  -- Shuffle, deal and show Cards
  ------------------------------------------------------------------------------------
  putStrLn "\nShuffling and Dealing cards..."
  game <- dealCards (shuffle newDeck)
  
  putStrLn "--------------------------------"
  putStrLn "AutoWars"
  putStrLn "--------------------------------\n"
  
  autowars game
  putStrLn "----------------------------------------------------------"
  putStrLn "Interactive Game"
  putStrLn "--------------------------------\n"
  putStrLn "You are the PlayerA and the computer is the PlayerB."
  interactiveWar game

  putStrLn "\n************************"
  putStrLn "End of Game \n"
