import Test.QuickCheck


power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)
------------------------------------------------------------------
-- Part 1

-- -- In order to calculate the power n k you take k+1 steps, one step for each calculation and
-- one additional since we start at 0.



----------------------------------------------------------
-- Part 2

power1 ::  Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product[x | x <-[n], y<-[1..k]]


--------------------------------------------------------------

-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n k
    | k == 0 = 1
    | even k = power2 (n*n) (div k 2)
    | otherwise = n * power2 n (k-1)

--------------------------------------------------------------

-- Part 4

{- A:  -- A Test when k > 0, test when k = 0 and n = 0 and some arbitrary numbers.
    We try (n,k) = [(10,10), (0,2), (2,0),(5,6),(2,8)]-}

-- B
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k == power1 n k) && (power1 n k == power2 n k)


-- C

myList = [(10,10), (0,2), (2,0),(5,6),(2,8)]

test_list_powers [] = True
test_list_powers ((x,y):xs) = prop_powers x y && test_list_powers xs

-- D
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = (power n k' == power1 n k') && (power1 n k' == power2 n k')
    where k' = abs k

