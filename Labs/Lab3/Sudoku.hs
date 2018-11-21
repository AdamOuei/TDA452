
import Test.QuickCheck
import Data.Char hiding (isDigit)
-------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))
    {-Sudoku [z | z <- [n], w<- [1..9]]
                where n = [x | x <- [Nothing],y<-[1..9]]-}

allFilledSudoku :: Sudoku
allFilledSudoku = Sudoku (replicate 9 (replicate 9 (Just 1)))
    {-Sudoku [z | z <- [n], w<- [1..9]]
                where n = [x | x <- [Nothing],y<-[1..9]]-}


-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) = length rows == 9 && checkSudoku rows isDigitOrNothing

-- Checks that each row is of size 9 and calls helpermethod to check that each element is empty or 1-9 
checkSudoku :: [[Maybe Int]] -> (Maybe Int -> Bool) -> Bool
checkSudoku [] _ = True
checkSudoku (row:rows) f = length row == 9 && checkRow row f && checkSudoku rows f

-- Checks that each element is either empty or between 1-9
checkRow :: [Maybe Int] -> (Maybe Int -> Bool) -> Bool
checkRow row f = and(map f row)

-- Helper method to check digit
isDigitOrNothing :: Maybe Int -> Bool
isDigitOrNothing Nothing = True
isDigitOrNothing number = number < Just 10 && number > Just 0

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = isSudoku (Sudoku rows) && checkSudoku rows isDigit

isDigit :: Maybe Int -> Bool
isDigit Nothing = False
isDigit number = number < Just 10 && number > Just 0

-------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rows) = printSudokuRows rows 

printSudokuRows :: [[Maybe Int]] -> IO ()
printSudokuRows [] = return ()
printSudokuRows (row:rows) =
    do putStr (unwords (printSudokuElement row) ++ "\n")
       printSudokuRows rows

printSudokuElement :: [Maybe Int] -> [String]
printSudokuElement = map makePrintElement 

makePrintElement :: Maybe Int -> String
makePrintElement element = case element of
    Nothing -> "."
    Just n -> show n

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
--readSudoku :: FilePath -> IO Sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku source =  do c <- readFile source 
                        return (convertListToSudoku $ map lines (lines c))
                        
                        

--removePunc xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'") ]


convertListToSudoku :: [[String]] -> Sudoku
convertListToSudoku list = Sudoku (map convertRowToSudoku list)

convertRowToSudoku :: [String] -> [Maybe Int]
convertRowToSudoku = map makeSudokuElement . map (:[]) . unwords


makeSudokuElement :: String -> Maybe Int
makeSudokuElement element = case element of
    "." -> Nothing
    n -> Just $ read n :: Maybe Int

-------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-------------------------------------------------------------------------