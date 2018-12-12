import System.IO(hFlush, stdout)
import Data.List(sort)
import Test.QuickCheck 
import Data.Maybe
import Data.Char(digitToInt)
-- 0 

--A

prompt p = do putStr p
              hFlush stdout    -- imported from System.IO
sumOfInts = do
    putStrLn "Write the number of ints to compute: "
    l <- readLn 
    let ask n = do 
                prompt "Enter a number: "
                readLn
    s <- mapM ask [1..l]
    putStr "The sum is: "
    print $ sum s

-- B 

sortInts = do l <- readUntilZero
              print $ sort l

readUntilZero = 
    do 
        prompt "Enter a number: "
        n <- readLn
        if n == 0 then return [] else do  ns <- readUntilZero 
                                          return (n:ns)

repeat' :: IO Bool -> IO () -> IO ()
repeat' test op = do op
                     b <- test
                     if b then return () else repeat' test op

prop_LookNothing:: Int -> [(Int,Char)]-> Property
prop_LookNothing element table = collect found $ isNothing(lookup element table) == not found
    where found = element `elem` map fst table

prop_LookJust :: Int -> [(Int,Char)]-> Property
prop_LookJust element table = case lookup element table of
    Just e -> label "Just" $ (element, e) `elem` table
    _ -> label "Nothing" $ True

prop_Look element table = prop_LookNothing element table .&&. prop_LookJust element table

game :: IO ()
game = do
    putStrLn "Think of a number between 1 and 100"
    putStrLn "Is it 50?"
    play 50

play :: Int -> IO ()
play lastGuess = do
    s <- getLine
    if s == "yes" then putStrLn "Great, I won"
    else 
        if s == "higher" then do 
            let guess = lastGuess + (100-lastGuess) `div` 2
            putStrLn ("Is it " ++ show guess ++ "?")
            play guess
        else do 
            let guess = (lastGuess) `div` 2
            putStrLn ("Is it " ++ show guess ++ "?")
            play guess
    
