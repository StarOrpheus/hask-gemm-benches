module Main where

import Data.Vector ( generateM, Vector, replicate, generate, (!))
import Data.Vector.Split (chunksOf)
import Control.Monad
import Criterion.Main
import System.Random

generateFloatMatrix n m = do
  d <- generateM (n*m) (\_ -> randomIO :: IO Float)
  return $ Matrix n m d

generateIntMatrix n m = do
  d <- generateM (n*m) (\_ -> randomIO :: IO Int)
  -- return $ Matrix n m d
  return $ Matrix n m (generate (n * m) id)

testN = 500
testM = 1000
testK = 500

data Matrix a = Matrix Int Int (Vector a)

matrixAt :: Matrix a
         -> Int
         -> Int
         -> a

matrixAt (Matrix n m d) i j = d ! (i * m + j)

-- | for the given N, M, K, matrixes A and B, and position pos, calculates (A*B)[pos]
gemmAtPoint :: (Num a)
            => Matrix a
            -> Matrix a
            -> Int -- Position to calculate
            -> a -- result

gemmAtPoint a b@(Matrix m k _) pos = do
  let i = pos `div` k
  let j = pos `mod` k
  let calc mid = matrixAt a i mid * matrixAt b mid j
  sum (map calc [0..(m - 1)])

main :: IO ()
main = do
  matrixA@(Matrix _ _ m1) <- generateIntMatrix testN testM
  matrixB@(Matrix _ _ m2) <- generateIntMatrix testM testK

  let fixedPointGemm = gemmAtPoint matrixA matrixB

  defaultMain [ bgroup "gemm-bench" [ bench "simple" $ whnf (generate (testN * testK)) fixedPointGemm]]
