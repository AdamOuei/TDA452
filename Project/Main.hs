import System.Random(randomRIO)
import Data.List(nub)
import Data.Set(insert,fromList,delete,Set,size,elemAt)

alphabetString  = map (:[]) "abcdefghijklmnopqrstuvwxyz"

main = do putStrLn "Welcome to the game!"
          putStrLn "Think of a word and write the amount of characters in the word:" 
          wordCount <- readLn 
          allWords <- getWords
          let filteredWords =  filter (\x -> length x == wordCount) allWords
          let setOfWords = fromList $ (filter (`elem` alphabetString ) . map (:[]) . unwords) filteredWords 
                        
          play wordCount setOfWords


play :: Int -> Set String -> IO ()
play wordCount setOfWords =
        do      
                putStrLn ['_' | l <- [1..wordCount] ]
                randomIndex <- randomRIO (0,size setOfWords-1)
                let guess = elemAt randomIndex setOfWords
                putStrLn ("Does your word contain the letter: " ++ guess ++ "? [y/n]")
                let newSet = delete guess setOfWords
                print newSet
                s<-getLine 
                if s == "y" 
                    then do print "At what position? (Specify if there are more than one"
                            position <- readLn
                            putStrLn [if l == position then head guess else '_' | l <- [1..wordCount] ]
                            play wordCount newSet
                else play wordCount newSet
                
                --putStrLn [if s == y then  else '_' | l <- [1..wordCount] ]
                --print $ 


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



-- TODO
--AritificalIntelligence :: AI -> AI




