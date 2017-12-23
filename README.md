# Haskell Implementations of Blackjack and War card games

## BlackJack Design
We can build the deck using our code for Lab exercise 5 – we can make a data type Number and a data type Suit. We can then make an Algebraic data type PlayingCards that is composed of a Card that has a Number and Suit. The type Deck will be a list of PlayingCards.
We can use System.Random to generate random numbers. At this point in time, we think we might be able to break the problem down and think about the functions we might need:
```haskell
showCard :: PlayingCards -> String
popCard :: Player -> PlayingCards (To remove card from the top of deck)
newDeck :: Deck (To generate a new Deck)
shuffleDeck :: Deck -> IO Deck
dealTwoCards :: Monad m => m Deck -> m (Player, Player, Deck) (To deal a pair of cards)
dealOneCard :: Player -> Deck -> (Player, Deck) (To deal just one card)
hit :: (Player, Deck) -> (Player, Deck)
doPlayerTurn :: (Player, Deck) -> IO (Player, Deck)
doDealerTurn :: (Player, Deck) -> IO (Player, Deck)
doTurns :: (Player, Player, Deck) -> IO () (A wrapper function that first calls doPlayerTurn followed by doDealerTurn).
cardValue :: PlayingCards -> Int (mapping card to its value)
cardValue2 :: PlayingCards -> Int (mapping card to its value)
```
After writing some pseudocode, we realized that some of the functions that we created such as popCard and showCard was not useful for implementation in BlackJack but was useful for war. Thus, we kept these functions.
We need a way to keep track of the scores. We decided to build functions that will sum the values of cards in a hand and check if the total is greater or less than 21. We will make use of Haskell’s inbuilt sum $ map to calculate the sum.
There were two values of Ace - 1 or 11. Therefore, we had to create two functions called cardValue and cardValue2 to take into account two different values of Ace. At this point we were not sure about the detailed implementation but it was only our design.
We will make the main function the IO type.

Haskell is functional which meant that we could not change the list containing the cards of the player. This was one of the different ways for us to this about as we were used to changing the same list for problem solving. During this design phase, we did some research and decided to return a tuple Game that contained player’ and dealer’ list.

## War
We can use the same idea as BlackJack to build the deck using our code for Lab exercise 5 – we made a data type Number and a data type Suit. We then created an Algebraic data type PlayingCards that is composed of a Card that has a Number and Suit. The type Deck is a list of PlayingCards.
Again, we used System.Random to generate random numbers. We took the same approach and started out thinking how to break the problem down and about the functions we might need:
```
showCard :: PlayingCards -> String
isSameCard :: PlayingCards -> PlayingCards -> Bool
isGreaterCard :: PlayingCards -> PlayingCards -> Bool
popCard :: Player -> PlayingCards
dealCards :: Monad m => m Deck -> m (Player, Player)
skipThreeCards :: Player -> Player
roundWinner :: Player -> Player -> IO (Player, Player)
```
We implemented `autowars :: (Player, Player) -> IO ()` as a recursive function that recursively calls itself until either Player have zero cards.
The implementation of  `interactiveWar :: (Player, Player) -> IO ()` is similar to autowars except interactiveWar waits for user input before continuing. 
We decided to design skipThreeCards so that it would drop 4 cards and then take 4 cards. We decided in our design that we will have a function that will determine the winner of each round by calling functions isSameCard and isGreaterCard on the cards given. This function is called roundWinner and seems to be where most of the ‘action’ is.
