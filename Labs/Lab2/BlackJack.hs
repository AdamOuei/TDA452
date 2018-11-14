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
    winner guest bank | gameOver guest || (value guest == value bank) || value bank > value guest = Bank
                      | value guest > value bank || gameOver bank = Guest
                      
    