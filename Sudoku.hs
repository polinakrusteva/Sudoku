module Sudoku where
import Data.Char
import Data.Matrix hiding (transpose)
import Test.QuickCheck
import Data.List
import Data.Maybe

data Sudoku = Sudoku [[Int]]

example :: Sudoku
example = Sudoku
      [ [3, 6, 0, 0, 7, 1, 2, 0, 0]
      , [0, 5, 0, 0, 0, 0, 1, 8, 0]
      , [0, 0, 9, 2, 0, 4, 7, 0, 0]
      , [0, 0, 0, 0, 1, 3, 0, 2, 8]
      , [4, 0, 0, 5, 0, 2, 0, 0, 9]
      , [2, 7, 0, 4, 6, 0, 0, 0, 0]
      , [0, 0, 5, 3, 0, 8, 9, 0, 0]
      , [0, 8, 3, 0, 0, 0, 0, 6, 0]
      , [0, 0, 7, 6, 9, 0, 0, 4, 3]
      ] 

allBlankSudoku::Sudoku
allBlankSudoku = Sudoku [replicate 9 0 | x<-[1..9]]

rows::Sudoku -> [[Int]]
rows (Sudoku xs) = xs

flattenSudoku::Sudoku -> [Int]
flattenSudoku (Sudoku xs) = foldr (++) [] xs

isSudoku::Sudoku -> Bool
isSudoku (Sudoku xs) 
    |length xs == 9 && (all (== 9) (map length xs)) && numbersCheck (Sudoku xs) = True
    |otherwise = False
      where
            numbersCheck (Sudoku xs) = all (>= 0) (flattenSudoku (Sudoku xs)) && all (<= 9) (flattenSudoku (Sudoku xs))

isSolved::Sudoku -> Bool
isSolved (Sudoku xs) = all (>= 1) (flattenSudoku (Sudoku xs)) && all (<= 9) (flattenSudoku (Sudoku xs))

sudokuToMatrix :: Sudoku -> Matrix Int
sudokuToMatrix (Sudoku xs) = fromLists (rows (Sudoku xs))

printSudoku::Sudoku -> IO()
printSudoku (Sudoku xs) = print (sudokuToMatrix (Sudoku xs))

cell::Gen(Int)
cell = frequency
         [(9, return 0),
         (1, do n <- choose (1,9)
                return n)]

instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku::Sudoku -> Bool
prop_Sudoku (Sudoku xs) = isSudoku (Sudoku xs) == True

type Block = [Int]

count::Eq a => a -> [a] -> Int
count x xs  = occurence x xs
    where
        occurence x l = sum[1|i<-l, i == x]

isOkayBlock::Block -> Bool
isOkayBlock block = all (== 1) [count i block| i<-block, i /= 0]

groupBy3 :: [t] -> [[t]]
groupBy3 (a:b:c:ds) = [a,b,c] : groupBy3 ds
groupBy3 [] = []
groupBy3 as = [as]

boxes :: [[a]] -> [[a]]
boxes = map concat . groupBy3 . concat . transpose . map groupBy3

blocks::Sudoku -> [Block]
blocks (Sudoku sud) = row ++ cols ++ block
      where
            row = rows (Sudoku sud)
            cols = rows (Sudoku (transpose sud))
            block = boxes sud

isOkay::Sudoku -> Bool
isOkay (Sudoku xs) = all (==True) [isOkayBlock x | x<-(blocks (Sudoku xs))]

type Pos = (Int, Int)

whichRow :: Sudoku -> Int
whichRow s = helper s 0
  where
    helper (Sudoku []) i = i
    helper (Sudoku (x:xs)) i = if isNothingPresent x 
        then 1 
        else helper (Sudoku xs) (i+1)

isNothingPresent :: Block -> Bool
isNothingPresent b = 0 `elem` b

whereIsNothing :: Block -> Int
whereIsNothing (x:xs) = if x == 0 then 1 else 1 + whereIsNothing xs

blank :: Sudoku -> Pos 
blank sud = (k -1, n-1)
  where
    k = whichRow sud
    n = whereIsNothing $ head $ drop (k-1) (rows sud)

blanks (Sudoku sud) = filter (\(x,y)->rows (Sudoku sud)!!(x)!!(y) == 0) [(i,j) | i<-[0..8], j<-[0..8]]

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs newEl = [xs !! i|i<-[0..(fst newEl - 1)]] ++ [(snd newEl)] ++ [xs !! j|j<-[(fst newEl + 1)..(length xs -1 )]]

update :: Sudoku -> Pos -> Int -> Sudoku
update sud (x, y) newEl = Sudoku (rows sud !!= (x, (rows sud !! x) !!= (y, newEl)))

solve::Sudoku -> Maybe Sudoku
solve s
      |isOkay s == False = Nothing
      |isSolved s = Just s
      |otherwise = pickASolution nineUpdatedSuds
            where
                  nineUpdatedSuds = [Just (update s (blank s) i)|i<-[1..9]]
                  --possibleSolutions = [solve (fromJust s') | s' <- nineUpdatedSuds]

pickASolution :: [Maybe Sudoku] -> Maybe Sudoku
pickASolution [] = Nothing
pickASolution (Just sud:suds)
      |isOkay sud = solve sud
      |otherwise = pickASolution suds