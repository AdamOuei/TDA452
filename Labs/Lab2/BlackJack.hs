module BlackJack where
import Cards
import RunGame

{- A0
The size is calculated by the function size where it calculates
size = (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))

This is calculated by the recursive call from the first card and then computing the size for the rest of the hand,

So basically it calulcates (Add (Card (Numeric 2) Hearts) Which is 1 card, then it does a call of size to 
(Add (Card Jack Spades) Empty)), it adds 1 card and then call on the empty hand which is 0, so the calulcations are

1 + 1 + 0 = 2,

-}

-- A1

hand2 = Add (Card Ace Hearts)(Add (Card Ace Spades) (Add (Card (Numeric 8) Hearts ) Empty))
hand1 = Add (Card Jack Hearts)(Add (Card Queen Spades) (Add (Card (Numeric 7) Hearts ) Empty))

empty :: Hand
empty  = Empty

--A2
value :: Hand -> Integer
value hand | hand == Empty = 0
           | valueWithValueOfAce 11 hand > 21  = valueWithValueOfAce 1 hand 
           | otherwise = initialValue hand

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) hand ) = 1 + numberOfAces hand
numberOfAces (Add _ hand) = numberOfAces hand

initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add card hand) = valueCard card + initialValue hand

valueWithValueOfAce :: Integer -> Hand -> Integer
valueWithValueOfAce n hand | n == 11 = initialValue hand                     
                           | n == 1 = initialValue hand - (10 * numberOfAces hand)
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric n) = n
valueRank face = 10

valueCard :: Card -> Integer
valueCard (Card rank suit) = valueRank rank

--A3
gameOver :: Hand -> Bool
gameOver hand | value hand > 21 = True
              | otherwise = False

--A4
winner :: Hand -> Hand -> Player
winner guest bank | value guest > 21 = Bank 
                  | value bank > 21 = Guest
                  | value guest > value bank = Guest
                  | value guest == value bank = Bank

-- B1
(<+) :: Hand -> Hand -> Hand
(<+) (Add c1 h1) (Add c2 h2) | h1 == Empty = Add c1 (Add c2 h2)
                             | otherwise = Add c1 ((<+) h1 (Add c2 h2))
 
