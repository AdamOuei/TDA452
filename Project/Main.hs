import System.Random(randomRIO)
import Data.List(nub)
import Data.Set(insert,fromList,delete,Set,size,elemAt)
import Test.QuickCheck
import Data.Maybe

alphabetString  = map (:[]) "abcdefghijklmnopqrstuvwxyz"

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
                s<-getLine 
                if s == "y" 
                    then do print "At what position? (Specify if there are more than one"
                            position <- readLn
                            let newWord = word !!= (position-1,head guess)
                                newWords = filterSet newWord filteredWords
                                newSetOfLetters = (delete guess . retrieveLetterSet) newWords
                        
                            print newSetOfLetters    
                            print newWords
                            play newWord newSetOfLetters newWords
                else play word newSet filteredWords
                

(!!=)  :: [a] -> (Int,a) ->[a]
list !!= (index, element) = start ++ element:end
                        where (start,_:end) = splitAt index list


getWords :: IO [String]
getWords = do  text <- readFile "Words.txt"
               let ls = lines text
               return ls

getRandomLetter :: Set String -> IO String
getRandomLetter set = do
                     randomIndex <- randomRIO (0,size set-1)
                     return $ elemAt randomIndex set


-- | Gives a set of letters that are in present in all the filtered words
retrieveLetterSet :: [String] -> Set String
retrieveLetterSet words = fromList filter'
                where filter' = filter (`elem` alphabetString ) letters
                      letters = (map (:[]) . unwords) words


filterSet :: String -> [String]  -> [String]
filterSet word  = checkWord (createTuples word) 


checkWord :: [(Int, Char)] -> [String] -> [String]
checkWord tuples@(x:xs) words | null words = []
                              | null xs =  filter (\y -> y !! fst x == snd x) words
                              | otherwise = checkWord xs (filter (\y -> y !! fst x == snd x) words)
                
                
                             
createTuples  :: String -> [(Int, Char)]
createTuples word = filter (\x -> snd x /= '_') $ [0..length word-1] `zip` word 


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




