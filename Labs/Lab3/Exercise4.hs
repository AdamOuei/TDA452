import System.IO(hFlush, stdout)
import Data.List(sort)
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

prop_LookNothing:: Maybe a -> Bool
prop_LookNothing element = isNothing(lookup element) 

prop_LookJust :: Maybe a -> Bool
prop_LookJust element = isJust(lookup element) 

prop_Look:: Maybe a -> Maybe a
prop_Look element | propLookNothing element = Nothing
                  | otherwise = Just (lookup element)