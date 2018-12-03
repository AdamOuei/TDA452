
-- 0
take' :: Int -> [a] -> [a]
take' n _      | n <= 0 =  []
take' _ []              =  []
take' n (x:xs)          =  x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' n list | n <= 0 = list
drop' _ []         = []
drop' n (x:xs)     = drop' (n-1) xs

splitAt' n xs = (take' n xs, drop' n xs)

splitAt'' :: Int -> [a] -> ([a],[a])
splitAt'' n list | n <= 0 = (list, [])
splitAt'' _ []         = ([],[])
splitAt'' n list     = (take' n list, drop' n list)

{--
zip3'' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3'' l1 l2 l3 = [(a,b,c)| a <- l1, b <- l2, c <- l3]
--}
zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] [] [] = []
zip3' (a:as) (b:bs) (c:cs) = (a,b,c): zip3' as bs cs

-- 6
{--
occursIn x (s:xs) |  x == s = True 
                  | otherwise = occursIn x xs

                  --}
occursIn' x xs = or [x == y | y <- xs]

allOccurIn' xs ys =  and [occursIn' x ys| x<-xs]

sameElements' xs ys = xs `allOccurIn` ys && ys `allOccurIn` xs
{--
allOccurIn:: Ord a => [a] -> [a] -> Bool
allOccurIn [] [] = True
allOccurIn [] _ = True
allOccurIn _ [] = True
allOccurIn (x:xs) ys |  not (occursIn' x ys) = False
                     | otherwise = allOccurIn xs ys

sameElements:: Ord a => [a] -> [a] -> Bool
sameElements [] [] = True
sameElements [] _ = False
sameElements _ [] = False
sameElements xs ys = xs `allOccurIn` ys && length xs == length ys

numOccurrences:: Ord a => a -> [a] -> Int
numOccurrences _ [] = 0
numOccurrences x (s:xs) | x == s = 1 + (numOccurrences x xs) 
                        | otherwise = numOccurrences x xs
                        --}

numOccurences' :: Eq a => a -> [a] -> Int
numOccurences' n xs = length [x |x <- xs, x == n]

-- 8

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x<-xs, y<-ys]

triples :: [(Integer,Integer,Integer)]
triples = [(a,b,c) | a<-[1..100], b<-[1..100], c<-[1..100], (a^2 + b^2 == c^2) ] 
