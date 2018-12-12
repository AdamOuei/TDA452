import System.Random(randomRIO)
import Data.List(nub,sort)
import Data.List.Split(splitOneOf)
import Data.Set(insert,fromList,delete,Set,size,elemAt, toList, intersection)
import Test.QuickCheck
import Data.Maybe


alphabetList  = map (:[]) "abcdefghijklmnopqrstuvwxyz"

main = do putStrLn "Welcome to the game!"
          putStrLn "Think of a word and write the amount of characters in the word:" 
          wordCount <- readLn 
          allWords <- getWords
          let filteredWords = filterWords (\x -> length x == wordCount) allWords
          let setOfLetters = retrieveLetterSet filteredWords 
          let word = ['_' | l<-[1..wordCount]]
          play word setOfLetters filteredWords


play :: String -> Set String-> [String] -> IO ()
play word setOfLetters filteredWords =
        do    
                putStrLn word
                randomIndex <- randomRIO (0,size setOfLetters-1)
                let guess = elemAt randomIndex setOfLetters
                putStrLn ("Does your word contain the letter: " ++ guess ++ "? [y/n]")
                let newSet = delete guess setOfLetters
                print newSet
                s<-getLine 
                if s == "y" 
                    then do print "At what position? (Specify if there are more than one"
                            input <- getLine

                            let parsedPositions = (map (\x -> read x ::Int) . splitOneOf ",;. ") input
                                correctIndexPositions = map (\x -> x-1) parsedPositions
                                zipPositions = zip correctIndexPositions $ repeat $ head guess
                                newWord = foldr (\x y -> (!!=) y x) word zipPositions  
                                newWords = filterSet newWord filteredWords
                                newSetOfLetters = delete guess $ newSet `intersection` retrieveLetterSet newWords 
                            print newWords
                            if length newWords == 1 then do 
                                putStrLn  ("Was the word you were thinking of: " ++ head newWords ++ "?")
                                answer <- getLine 
                                if answer == "y" then 
                                        putStrLn "Thanks for playing"
                                else do
                                        --Look at lazy evaluation lecture
                                        putStrLn "What was the word you were thinking of?"
                                        theWord <- getLine
                                        wordList <- getWords
                                        let sortedWords = (unlines . sort) (wordList ++ [theWord])
                                        writeFile "Words.txt" sortedWords   
                                else play newWord newSetOfLetters newWords
                else play word newSet filteredWords
                
-- | Inserts an element a at a given position int in a list and returns the new list
(!!=)  :: [a] -> (Int,a) ->[a]
list !!= (index, element) | index < length list && index >= 0 = start ++ element:end
                          | otherwise = error "Out of bounds"
                        where (start,_:end) = splitAt index list

-- | Gets the words from a file with a word list
getWords :: IO [String]
getWords = do  text <- readFile "Words.txt"
               let ls = lines text
               return ls

-- | Given a set of letters it returns a random letter from that set
getRandomLetter :: Set String -> IO String
getRandomLetter set = do
                     randomIndex <- randomRIO (0,size set-1)
                     return $ elemAt randomIndex set


-- | Gives a set of letters that are in present in all the filtered words
retrieveLetterSet :: [String] -> Set String
retrieveLetterSet words = fromList filter'
                where filter' = filter (`elem` alphabetList ) letters
                      letters = (map (:[]) . unwords) words

-- | Filters a set on a word using a help function
filterSet :: String -> [String]  -> [String]
filterSet word  = checkWord (createTuples word) 

-- | Given a tuple of positions and characters and a wordlist we filter the wordlist on the occurences of the positions and characters
checkWord :: [(Int, Char)] -> [String] -> [String]
checkWord tuples@(x:xs) words | null words = []
                              | null xs =  filter (\y -> y !! fst x == snd x) words
                              | otherwise = checkWord xs (filter (\y -> y !! fst x == snd x) words)
                
                
 -- | Creates a tuple of non-empty characters and the position of those                           
createTuples  :: String -> [(Int, Char)]
createTuples word = filter (\x -> snd x /= '_') $ [0..length word-1] `zip` word 

-- TODO 
winner :: String
winner = undefined

-- TODO
gameOver :: String
gameOver = undefined
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
