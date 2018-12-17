import System.Random(randomRIO)
import Data.List(nub,sort)
import Data.List.Split(splitOneOf)
import Data.Set(insert,fromList,delete,Set,size,elemAt, toList, intersection)
import Test.QuickCheck
import Data.Maybe
import Text.Read
import Data.Char

alphabetList  = map (:[]) "abcdefghijklmnopqrstuvwxyz"
frequencyList = map (:[]) "etaoinsrhldcumfpgwybvkxjqz"


main = do putStrLn "Welcome to the game!"
          allWords <- getWords
          characterAmount <- getLineInt
          let 
            filteredWords = filterWords (\x -> length x == characterAmount) allWords
            setOfLetters = retrieveLetterSet filteredWords alphabetList
            word = ['_' | l<-[1..characterAmount]]
            in
            play word setOfLetters filteredWords
              
            

play :: String -> Set String-> [String] -> IO ()
play word setOfLetters filteredWords =
        do    
                putStrLn word
                guess <- getRandomLetter setOfLetters
                putStrLn ("Does your word contain the letter: " ++ guess ++ "? [y/n]")
                let newSet = delete guess setOfLetters
                --print newSet
                s<-getLine 
                case s  of
                        -- TODO Handle wrong input like we did in getLineInt
                  "y" ->  do    putStrLn "At what position? (Specify if there are more than one)"
                                input <- getLine
                                let
                                    newWord = foldr (\x y -> (!!=) y x) word $ getPositions input guess 
                                    newWords = filterSet newWord filteredWords
                                    newSetOfLetters = delete guess $ newSet `intersection` retrieveLetterSet newWords alphabetList
                                if length newWords <= 1 then
                                        if null newWords then
                                                putStrLn"No such word"
                                        else gameOver newWords
                                else play newWord newSetOfLetters newWords
                  "n" -> play word newSet filteredWords
                  _ -> play word setOfLetters filteredWords
                
-- | Reads the input from the user, if not an int it prompts for another input
getLineInt :: IO Int
getLineInt  =
        do putStrLn "Think of a word and write the amount of characters in the word:" 
           wordCount <- getLine
           case readMaybe wordCount of
                Just x -> return x
                Nothing -> putStrLn "Invalid number entered" >> getLineInt 
                
-- TODO: fråga thomas
-- | Returns the positions for the guess as a tuple (position, guess)
getPositions :: String -> String -> [(Int,Char)]
getPositions input guess = zip correctIndexPositions $ repeat $ head guess 
        where parsedPositions = (map (\x -> read x ::Int) . splitOneOf ",;. ") input
              correctIndexPositions = map (\x -> x-1) parsedPositions 
    
        
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
getWords = do  text <- readFile "./Words.txt"
               let ls = lines text
               return ls

-- | TODO Keep or don't keep               
prop_getWords ::  IO Bool
prop_getWords = do file <- readFile "./Words.txt"
                   let words = lines file
                   otherWord <- getWords
                   return $ otherWord == words

-- | Not working, solve later
addNewWord = do theWord <- getLine
                wordList <- getWords
                let sortedWords = (unlines . sort) (wordList ++ [theWord])
                writeFile "Words.txt" sortedWords 

-- | Given a set of letters it returns a random letter from that set
getRandomLetter :: Set String -> IO String
getRandomLetter set = do
                     randomIndex <- randomRIO (0,size set-1)
                     return $ elemAt randomIndex set


-- | Gives a set of letters that are in present in all the filtered words
retrieveLetterSet :: [String] -> [String] -> Set String
retrieveLetterSet words charList = fromList filter'
                where filter' = filter (`elem` charList ) letters
                      letters = (map (:[]) . unwords) words

-- | Checks so the resulting letter set all is present in the char list
prop_retrieveLetterSet :: [String] -> [String] -> Bool
prop_retrieveLetterSet words charList = all (`elem` charList) resultingSet
        where resultingSet = retrieveLetterSet words charList


-- | Filters a set on a word using a help function
filterSet :: String -> [String]  -> [String]
filterSet word  = checkWord (createTuples word) 

-- | Given a tuple of positions and characters and a wordlist we filter the wordlist on the occurences of the positions and characters
checkWord :: [(Int, Char)] -> [String] -> [String]
checkWord tuples@(x:xs) words | null words = []
                              | null xs =  filter (\y -> y !! fst x == snd x) words
                              | otherwise = checkWord xs (filter (\y -> y !! fst x == snd x) words)

-- | Checks so all constructed tuples of a word actually contains the character of the word in the 
--   correct position for all results in the set of words
prop_checkWord :: String -> [String] -> Property
prop_checkWord word wordList = not (null word) && not  (null wordList) ==> 
                               all (\x ->  all (\y -> x !! fst y  == snd y) tuples) result
        where result = checkWord tuples filteredWords
              tuples = createTuples word
              filteredWords = filter (\x -> length word == length x) wordList
                
                
 -- | Creates a tuple of non-empty characters and the position of those                           
createTuples  :: String -> [(Int, Char)]
createTuples word = filter (\x -> snd x /= '_') $ [0..length word-1] `zip` word

prop_createTuples_correct :: String -> Bool
prop_createTuples_correct string = length removeBlanks == length (createTuples string)
                where removeBlanks = filter (`notElem` ['_']) string 

-- | Check if the last word in the list is the word the player was thinking about, 
-- otherwise it asks what you were thinking about
gameOver :: [String] -> IO ()
gameOver newWords@(x:xs) =
                do 
                putStrLn  ("Was the word you were thinking of: " ++ x ++ "?")
                answer <- getLine 
                if answer == "y" then 
                 putStrLn "Thanks for playing"
                else putStrLn "What was the word you were thinking of?"
                
                
-- | Filters words according to input                     
filterWords :: (String -> Bool) ->[String] -> [String]
filterWords  = filter

-- | Checks that all the words after the filtering is filtered with the correct length
prop_filterWords :: Int -> [String] -> Bool
prop_filterWords wordLength = all checkLength . filterWords checkLength
        where
          checkLength x = length x == wordLength
-- TODO
--AritificalIntelligence :: AI -> AI
