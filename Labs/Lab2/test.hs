module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

--1  
maxi:: Integer -> Integer -> Integer
maxi x y | x >= y = x 
         | otherwise = y


prop_maxi :: Integer -> Integer -> Bool
prop_maxi x y = max x y == maxi x y

--2 
sumsq :: Integer -> Integer
sumsq 1 = 1
sumsq n = sumsq(n-1) + n^2

prop_sumsq :: Integer -> Bool
prop_sumsq n = sumsq n == product n 
    where product n = ((n* (n+1) * (2*n + 1)) `div` 6)

--3
hanoi :: Integer -> Integer
hanoi 0 = 0 
hanoi n = 2 * hanoi(n-1)+1

--4 
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib(n-2) + fib(n-1)

--5
smallestFactor :: Integer -> Integer
smallestFactor 1 = 1
smallestFactor n = nextFactor 1 n

nextFactor :: Integer -> Integer -> Integer
nextFactor k n | k > n = error "Cannot divide by a larger number"
nextFactor k n | (n `mod` (k+1)) == 0 = k+1
               | otherwise = nextFactor (k+1) n



prop_sf1 :: Integer -> Integer -> Bool
--Check that the Integer is divisible with the other one
prop_sf1 n t = (n `mod` t == 0)

prop_sf2 :: Integer -> Integer -> Bool
-- Check that it is smaller than what we divide 
prop_sf2 n t = n `mod` t == 0 && n `div` t > n

numFactors :: Integer -> Integer
numFactors 0 = 0
numFactors n = (nextFactor 1 n) + 1 

--6
multiply :: Num a => [a] -> a
multiply [] = 1
--multiply [a] = a
multiply (a: as) = a * multiply as

--7 
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (a:as) | elem a as = True 
                  | otherwise = duplicates as

