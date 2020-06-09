module Lm.LinearEquations
( gaussianElimination,
  gsElimStep,
  genLinearSys,
  LinearSystem (..),
  backSub,
  completePivotPS,
  maxAbsPos,
  gaussianCP,
  gcpSolve
) where

import Lm.Vector
import Lm.Matrix
import Debug.Trace
import Lm.Helpers (appendCol,swapRows)

data LinearSystem = LinearSystem { augMat :: Matrix
                                 } deriving (Show)

-- | Creates a linear system from a matrix of cofactors and a vector
-- representing the right hand side of the linear equations
genLinearSys :: Matrix -> Vector -> LinearSystem
genLinearSys mat v
  | rows mat == dim v =
    let matDat = appendCol (dat mat) (vals v)
    in LinearSystem $ Matrix matDat (rows mat) (cols mat + 1)

-- | Performs gaussian elimination on the linear system
gaussianElimination :: Matrix -> Matrix
gaussianElimination mat = foldl (\acc x -> gsElimStep acc x) mat [1..rows mat]

-- | Performs the i'th step of gaussian elimination
gsElimStep :: Matrix -> Int -> Matrix
gsElimStep (Matrix d r c) step =
  let toBeZeroed = drop step (colFromList d (step -1))
      scalars = (/) <$> toBeZeroed <*> [d !! (step -1) !! (step -1)]
      rowStep = d !! (step - 1)
      new_d = [if i >= step then
        let
          zeroPad = take step (repeat 0)
          rowStepAdjNonZero = (*) <$> (drop step rowStep) <*> [scalars !! (i - step)]
          curRowNonZero = drop step (d !! i)
        in zeroPad ++ zipWith (-) curRowNonZero rowStepAdjNonZero
--          zipWith (-) (d !! i) ((*) <$> rowStep <*> [scalars !! (i - step)])
        else (d !! (i)) | i <- [0..r - 1]]
  in Matrix new_d r c

-- | Perfoms backward substituion on a augmented matrix in row echelon form
backSub :: Matrix -> Vector
backSub (Matrix d r c) =
  Vector (foldr (
    \i acc -> let
      -- Evaulate the LHS of equation with (now) known components of solution
      evaledCoeffs = foldl (\acc' j -> acc' + (d !! i !! j)*(acc !! (j-i-1))) 0 [i+1..r-1]
      rhs = (d !! i !! (c-1)) - evaledCoeffs
      -- Calculate the new component of the solution and then prepend it to the accumulator
      -- which is the solution vector
      xSolv = (rhs) / (d !! i !! i)
      in xSolv : acc
  ) [] [0..r -1]) r

-- | Finds the position of the largest element in a 2d list. Returns form (Row,Column)
maxAbsPos ::  [[Float]] -> (Int,Int)
maxAbsPos d = let
  maxPairs = foldr (\i acc -> let
    -- Makes list of [position,value] for largest item in each row
    maxPosInR = foldl (
        \val j -> if (abs (d !! i !! j)) > (val !! 1) then [fromIntegral j,abs(d !! i !! j)] else val
      ) [0,abs(d !! i !! 0)] [0..(length (d !! i))-1 ]
    in maxPosInR:acc
    ) [] [0..length d - 1]
  maxPosRow = foldl (
      \acc row -> if (maxPairs !! row !! 1) > (maxPairs !! acc !! 1) then row else acc
    ) 0 [0..length maxPairs - 1]
  in (maxPosRow,round $ maxPairs !! maxPosRow !! 0)

-- | Solves system of linear equations with gaussian elimination + complete pivvoting
gcpSolve :: Matrix -> Matrix
gcpSolve mat
  | rows mat == (cols mat) - 1 = let
    gsResult = gaussianCP mat
    (reFormMat,colMat) = (gsResult !! 0, gsResult !! 1)
    xHat = backSub reFormMat
    in mProduct colMat (mTranspose $ matFromList [vals xHat])

-- | Performs Gaussian elimination with complete pivoting
gaussianCP :: Matrix -> [Matrix]
gaussianCP mat =
  foldl (\acc x -> let
    (pivoted,permRowMat) = completePivotPS (acc !! 0) x
    newMat = gsElimStep pivoted x
    in [newMat,mProduct  (acc !! 1) permRowMat]
  ) [mat,idMat (rows mat)] [1..rows mat]

-- | Performs the pivoting step for Gaussian elimination with complete pivoting
completePivotPS :: Matrix -> Int -> (Matrix,Matrix)
completePivotPS (Matrix d r c) step = let
  subMat  = foldr (
    \i acc -> let
      newR = (foldr (\j subRow -> (d !! i !! j):subRow) [] [step - 1..r-1])
      in newR:acc
    ) [] [step - 1 .. r - 1]
  (rowSwitch,colSwitch) = maxAbsPos subMat
  -- Refactor with permeutation matricies?
  rowSwitched = matFromList $ swapRows d (step - 1) (rowSwitch + step - 1)
  colSwitched = mTranspose $ matFromList $ swapRows (dat (mTranspose rowSwitched)) (step -1) (colSwitch + step - 1)
  in (colSwitched,permMat r (step - 1) (colSwitch + step - 1))