module Lm.Vector
( Vector (..),
  dotProd,
  vecAbs,
  crossProd,
  vecFromList
) where

data Vector = Vector { vals :: [Float]
                     , dim :: Int
                     } deriving (Show)

dotProd :: Vector -> Vector -> Float
dotProd (Vector dat_a dim_a) (Vector dat_b dim_b)
  | dim_a == dim_b = foldl (\acc i -> acc + (dat_a !! i) * (dat_b !! i)) 0 [0..dim_a - 1]
  | otherwise = error "Vector dimensions not equal"

vecAbs :: Vector -> Float
vecAbs (Vector dat _) =  sqrt $ foldl (\acc x -> acc + x^2) 0 dat

crossProd :: Vector -> Vector -> Maybe Vector
crossProd a b
  | dim a == dim b = Just $ Vector [cpComp a b 1 2, - cpComp a b 0 2, cpComp a b 0 1] 3
  | otherwise = Nothing

cpComp :: Vector -> Vector -> Int -> Int -> Float
cpComp a b i j = vals a !! i * vals b !! j - vals a !! j * vals b !! i

vecFromList :: [Float] -> Vector
vecFromList [] = error "Empty List"
vecFromList x = Vector x (length x)

