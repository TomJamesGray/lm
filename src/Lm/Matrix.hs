module Lm.Matrix (
    Matrix (..),
    mProduct,
    mTranspose,
    matFromList,
    colFromList,
    idMat,
    permMat
) where

import Lm.Vector (vecFromList,Vector,dotProd)
import Lm.Helpers (digits,padNumber,swapRows)
import Text.Printf
import Data.List hiding (transpose)

data Matrix = Matrix {
  dat :: [[Float]]
  , rows :: Int
  , cols :: Int
  } deriving (Eq)

instance Show Matrix where
  show a = let chars = maximum $ map (maximum . map digits) (dat a)
               r = map (map (padNumber chars)) (dat a)
               r' = map unwords r
           in intercalate "\n" r'

mProduct :: Matrix -> Matrix -> Matrix
mProduct a b
  | cols a == rows b = let matDat = map (mProductRow a b) [0..rows a - 1]
                       in Matrix matDat (rows a) (cols b)
  | otherwise = error "Incorrect Matrix Dimensions"

mTranspose :: Matrix -> Matrix
mTranspose (Matrix d r c) = matFromList $ map (colFromList d) [0..c - 1]


matFromList :: [[Float]] -> Matrix
matFromList dat = let row = length dat
                      col = length $ dat !! 0
                      neqCol = foldl (\acc x -> if (length x) /= col then True else acc) False dat
                  in  if not neqCol then Matrix dat row col else error "Dimensions not consistent"

mProductRow :: Matrix -> Matrix -> Int -> [Float]
mProductRow a b row = map (mProductElem a b row) [0..cols b -1]

mProductElem :: Matrix -> Matrix -> Int -> Int -> Float
mProductElem (Matrix a _ _) (Matrix b _ _) i j = 
  let r = vecFromList ( a !! i)
      c = vecFromList $ colFromList b j
  in dotProd r c

colFromList :: [[Float]] -> Int -> [Float]
colFromList d col = foldr (\x acc -> (x !! col) : acc) [] d

-- | Interchange the rows on a matrix by using permeutation matricies
--rowInterchange :: Matrix -> Int -> Int -> Matrix
--rowInterchange (Matrix d c) r1 r2 = let


-- | Create the square nxn permeutation matrix made by interchanging
-- rows r1 and r2 in the nxn identity matrix
permMat :: Int -> Int -> Int -> Matrix
permMat n r1 r2 = Matrix (swapRows (dat (idMat n)) r1 r2) n n


-- | Create the nxn identity matrix
idMat :: Int -> Matrix
idMat n = let
  dat = foldl (
    \acc i -> let
      newR = take i (repeat 0) ++ [1] ++ take (n-i-1) (repeat 0)
      in acc ++ [newR]
    ) [] [0..n-1]
  in Matrix dat n n