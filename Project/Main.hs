import Data.List.Split(splitOneOf)
import Data.Set(insert,fromList,delete,Set,size,elemAt, toList, intersection)
import Test.QuickCheck
import Data.Maybe
import Text.Read
import Data.Char

frequencyList = "etaoinsrhldcumfpgwybvkxjqz"

main = do putStrLn "Welcome to the game!"
          allWords <- getWords
          characterAmount <- getLineInt
          let 
            filteredWords = filter (\x -> length x == characterAmount) allWords
            setOfLetters = retrieveLetterSet filteredWords 
            word = ['_' | l<-[1..characterAmount]]
            in
            play word setOfLetters filteredWords
                         

play :: String -> Set Char-> [String] -> IO ()
play word setOfLetters filteredWords =
        do    
                putStrLn word
                guess <- getRandomLetter setOfLetters
                putStrLn ("Does your word contain the letter: " ++ [guess] ++ "? [y/n]")
                let newSet = delete guess setOfLetters
                s<-getLine 
                case s  of
                  "y" ->  do    input <- handleInput
                                if notOutOfBounds input word then
                                 let
                                    newWord = foldr (\x y -> (!!=) y x) word $ getPositions input guess 
                                    newWords = filterList newWord filteredWords
                                    newSetOfLetters = delete guess $ newSet `intersection` retrieveLetterSet newWords
                                    in 
                                    if length newWords <= 1 then
                                        if null newWords then
                                                putStrLn"No such word"
                                        else gameOver newWords
                                    else play newWord newSetOfLetters newWords
                                else putStrLn "Not a valid position, try again" >> play word newSet filteredWords       
                  "n" -> play word newSet filteredWords
                  _ -> play word setOfLetters filteredWords


-- TODO: maybe make the end of game better

-- | Makes sure that the input is correct otherwise calls the function so the user can try again
handleInput :: IO [Int]
handleInput = do 
        putStrLn "At what position? (Specify if there is more than one)"
        input <- getLine
        case mapM readMaybeInt $ splitInput input of
                Just x -> return x
                Nothing -> putStrLn "Not a valid input" >> handleInput
        where splitInput = splitOneOf ",;. " 

-- | Checks so that the input does not contain a position larger than the word
notOutOfBounds :: [Int] -> String -> Bool
notOutOfBounds guessInput word = all  ( \x -> x <= wordLength && x /= 0) guessInput
                where wordLength = length word

-- | Tests that the notOutOfBounds function is working as intended
prop_inBounds ::[Int] -> String -> Property
prop_inBounds list word = not(null list) && not(null word) ==>  
                notOutOfBounds list word ==  not (any (\x -> x > length word || x == 0) list)  

                
                
-- | Reads the input from the user, if not a number it prompts for another input
getLineInt :: IO Int
getLineInt  =
        do putStrLn "Think of a word and write the amount of characters in the word:" 
           wordCount <- getLine
           case readMaybe wordCount of
                Just x -> return x
                Nothing -> putStrLn "Invalid number entered" >> getLineInt 
                
-- | Returns the positions for the guess as a tuple (position, guess)
getPositions :: [Int] -> Char -> [(Int,Char)]
getPositions input guess = zip correctIndexPositions $ repeat guess 
        where correctIndexPositions = map (\x -> x-1) input     
        
-- | Test for the function getPosition so that it correctly gives tuples with the correct int and char
prop_getPositions_correct :: [Int] -> Char -> Bool
prop_getPositions_correct list char =  isSameLength && isSameList && isCorrectChar
        where tuples = getPositions list char
              getInts = map fst tuples
              convertIndex = map (+1) getInts  
              isSameList = convertIndex == list                
              isCorrectChar = all (\(x,y) -> y == char) tuples
              isSameLength = length list == length convertIndex

-- | Helper function to help parse input with more than one position
readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe
    
        
-- | Inserts an element a at a given position int in a list and returns the new list
(!!=)  :: [a] -> (Int,a) ->[a]
list !!= (index, element) | index < length list && index >= 0 = start ++ element:end
                          | otherwise = error "Out of bounds"
                        where (start,_:end) = splitAt index list

-- | Checks that the lists have the same length after inserting a new 
-- element and that the inserted element is in the index it was inserted                    
prop_listInsertion_correct :: [Int] -> (Int,Int) -> Property
prop_listInsertion_correct list (index, element) = index <= length list-1  && index > 0 ==>
                                                    newList !! index == element
                                                    && length list == length newList
                                        where newList = list !!= (index, element)

-- | Gets the words from a file with a word list
getWords :: IO [String]
getWords = do  text <- readFile "/usr/share/dict/words"
               let ls = lines text
               return ls

-- | Generates a random letter with a higher frequency of a more common letter
getRandomLetter :: Set Char -> IO Char
getRandomLetter set = generate $ frequency zipped 
                where zipped = [1..] `zip`  (reverse . map return) result
                      result = filter (`elem` setToList) frequencyList
                      setToList = toList set
                      

-- | Gives a set of letters that are in present in all the filtered words
retrieveLetterSet :: [String] -> Set Char
retrieveLetterSet =  fromList . map toLower . concat


-- | Checks so the resulting letter set all is present in the char list
prop_retrieveLetterSet :: [String] -> Bool
prop_retrieveLetterSet words = all (`elem` listFromSet) concatedWords
        where resultingSet = retrieveLetterSet words
              listFromSet = toList resultingSet
              concatedWords = (map toLower .concat) words


-- | Filters a set on a word using a help function check word to filter on the filled positions in the guess
filterList :: String -> [String] -> [String]
filterList word  = filterWordsWithTuples (createTuples word) 

-- | Given a tuple of positions and characters and a wordlist we filter the wordlist on the occurences of the positions and characters
filterWordsWithTuples :: [(Int, Char)] -> [String] -> [String]
filterWordsWithTuples tuples@(x:xs) words | null words = []
                              | null xs =  filter (\y -> y !! fst x == snd x) words
                              | otherwise = filterWordsWithTuples xs (filter (\y -> y !! fst x == snd x) words)

-- | Checks so all constructed tuples of a word actually contains the character of the word in the 
--   correct position for all results in the set of words
prop_filterWordsWithTuples:: String -> [String] -> Property
prop_filterWordsWithTuples word wordList = not (null word) && not (null wordList) ==> 
                               all (\x ->  all (\y -> x !! fst y  == snd y) tuples) result
        where result = filterWordsWithTuples tuples filteredWords
              tuples = createTuples word
              filteredWords = filter (\x -> length word == length x) wordList
                
                
 -- | Creates a tuple of non-empty characters and the position of those                           
createTuples  :: String -> [(Int, Char)]
createTuples word = filter (\x -> snd x /= '_') $ [0..length word-1] `zip` word

-- | Checks so that the function createTuples returns the correct tuple list
prop_createTuples_correct :: String -> Bool
prop_createTuples_correct string = length removeBlanks == length (createTuples string)
                where removeBlanks = filter (`notElem` ['_']) string 

-- | Check if the last word in the list is the word the player was thinking about, 
-- otherwise it asks what you were thinking about
gameOver :: [String] -> IO ()
gameOver (x:_) =
                do 
                putStrLn  ("Was the word you were thinking of: " ++ x ++ "?")
                answer <- getLine 
                if answer == "y" then 
                 putStrLn "Thanks for playing"
                else putStrLn "Aww too bad, play again! :D"
                

