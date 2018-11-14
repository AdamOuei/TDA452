module BlackJack where
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import System.Random

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
    
-- | Returns an empty hand
empty :: Hand
empty  = Empty
    
    --A2

-- | Calculates the value of the given hand
value :: Hand -> Integer
value hand | hand == Empty = 0
           | valueWithValueOfAce 11 hand > 21  = valueWithValueOfAce 1 hand 
           | otherwise = initialValue hand
    
-- | Calculates the number of aces in a given hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) hand ) = 1 + numberOfAces hand
numberOfAces (Add _ hand) = numberOfAces hand
    
-- | Calculates the value of the hand with the default value of the Ace card (11)
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add card hand) = valueCard card + initialValue hand

-- | Computes the value of the hand given the hand and number of aces
valueWithValueOfAce :: Integer -> Hand -> Integer
valueWithValueOfAce n hand | n == 11 = initialValue hand                     
                           | n == 1 = initialValue hand - (10 * numberOfAces hand)
-- | Helper function to determine the value of the rank (2-11) depending on the card                           
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric n) = n
valueRank face = 10

-- | Uses the helper function to return the value of a card 
valueCard :: Card -> Integer
valueCard (Card rank suit) = valueRank rank

--A3
-- | Checks if the value of the hand is above 21 then it is game over (True)
gameOver :: Hand -> Bool
gameOver hand | value hand > 21 = True
              | otherwise = False

--A4

-- | Determines who wins between the bank and the player
winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest 
                  || (value guest == value bank) 
                  || (value bank > value guest && not(gameOver bank)) = Bank
                  | value guest > value bank
                  || gameOver bank 
                  || value guest == 21 = Guest
                      
    

-- B1
(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty = Empty
(<+) Empty hand2 = hand2
(<+) (Add c1 Empty) hand2 = Add c1 hand2
(<+) (Add c1 hand) hand2 = Add c1 ( hand <+ hand2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3 


prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size p1 + size p2 == size ( p1 <+ p2) 

--B2
fullDeck :: Hand
fullDeck = ((chooseSuit Diamonds <+ chooseSuit Clubs) <+ chooseSuit Hearts) <+ chooseSuit Spades

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
                    [Add (Card {rank = n, suit = suit}) Empty| n<- map Numeric [2..10] 
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

shuffle :: StdGen -> Hand -> Hand
shuffle g deck  | deck == empty = empty
                | otherwise = card <+ shuffle g' cdeck
                 where(cdeck, card) = pickNthCard deck n
                      (n,g') = randomR (1, size deck) g
 

pickNthCard:: Hand -> Integer -> (Hand,Hand)
pickNthCard deck n | n == 1 = ( currentDeck , currentCard)
                   | otherwise = ( currentCard <+ returnDeck, returnCard)
                    where (currentDeck, currentCard) = draw deck empty
                          (returnDeck, returnCard) = pickNthCard currentDeck (n-1)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h


belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size (shuffle g h) == size h

implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation

