module Lm.Helpers
( digits,
  padNumber,
  appendCol,
  swapRows
) where

digits :: (Num a,Show a) => a -> Int
digits x = length $ show x

padNumber :: (Num a,Show a) => Int -> a -> String
padNumber size a
  | digits a == size = show a
  | digits a < size = (concat $ replicate (size - digits a) " ") ++ (show a)
  | otherwise = error "Size required less than digits"

appendCol :: [[a]] -> [a] -> [[a]]
appendCol list col
  | length list == length col = foldl (\acc i -> acc ++ [(list !! i) ++ [col !! i]]) [] [0..length list -1]
  | otherwise = error "Incorrect dimensions"

swapRows :: [[a]] -> Int -> Int -> [[a]]
swapRows d r1 r2
  | r1 == r2 = d
  | r1 < (length d) && r2 < (length d) = let
    r1Init = d !! r1
    r2Init = d !! r2
    in [if i == r1 then r2Init else (if i == r2 then r1Init else d !! i) | i<-[0..length d -1]]