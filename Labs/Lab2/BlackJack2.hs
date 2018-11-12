module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

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
hand1 = Add(Card Ace Clubs)(Add (Card Jack Hearts)(Add (Card Queen Spades) (Add (Card (Numeric 7) Hearts ) Empty)))
deck = fulldeck

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
(<+) Empty Empty = Empty
(<+) Empty hand2 = hand2
(<+) (Add c1 Empty) hand2 = Add c1 hand2
(<+) (Add c1 hand) hand2 = Add c1 ((<+) hand hand2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3 


prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size p1 + size p2 == size ((<+) p1 p2) 

--B2
fulldeck :: Hand
fulldeck = ((<+) ((<+) ((<+) (chooseSuit Diamonds) (chooseSuit Clubs)) (chooseSuit Hearts)) (chooseSuit Spades))

chooseSuit :: Suit -> Hand
chooseSuit suit =
                case suit of
                Spades   -> createSuit Spades
                Hearts   -> createSuit Hearts
                Diamonds -> createSuit Diamonds
                Clubs    -> createSuit Clubs

createSuit :: Suit -> Hand 
createSuit suit = foldl 
                    (<+)
                    Empty 
                    [(Add (Card {rank = n, suit = suit}) Empty)| n<- map Numeric [2..10] 
                    ++[Jack,Queen,King,Ace]]

--B3
draw :: Hand -> Hand -> (Hand,Hand)
draw deck hand | deck == empty = error "draw: The deck is empty"
draw (Add c1 h1) hand | hand == empty = (h1, Add c1 empty)
draw (Add c1 h1) (Add c2 h2) = (h1 , Add c1 (Add c2 h2))

--B4
playBank :: Hand -> Hand
playBank deck = playBank' deck empty


playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand' >= 16 = bankHand'
                        | otherwise = playBank' deck' bankHand'
            where (deck',bankHand') = draw deck bankHand

--B5          