import System.Random(randomRIO)
import Data.List(nub)
import Data.Set(insert,fromList,delete,Set,size,elemAt)

main = play


play :: IO ()
play =
        do      putStrLn "Welcome to the game!"
                putStrLn "Think of a word and write the amount of characters in the word:"        
                wordCount <- readLn 
                putStrLn ['_' | l <- [1..wordCount] ]
                allWords <- getWords
                let filteredWords =  filter (\x -> length x == wordCount) allWords
                let setOfWords = fromList $ (filter (/= " ") . map (:[]) . unwords) filteredWords 
                print filteredWords
                randomIndex <- randomRIO (0,size setOfWords-1)
                let guess = elemAt randomIndex setOfWords          
                print $ delete guess setOfWords 
            


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




