import System.Random(randomRIO)
import Data.List(nub)
import Data.Set

main = play


play :: IO ()
play =
        do      putStrLn "Welcome to the game!"
                putStrLn "Think of a word and write the amount of characters in the word:"        
                wordCount <- readLn 
                putStrLn ['_' | l <- [1..wordCount] ]
                allWords <- getWords
                print $ filter (\x -> length x == wordCount) allWords

            
--parseList :: [String]
--parseList = filter 

getWords :: IO [String]
getWords = do  text <- readFile "Words.txt"
               let ls = lines text
               return ls

getRandomQuestion :: IO String
getRandomQuestion = undefined


-- TODO
--AritificalIntelligence :: AI -> AI




