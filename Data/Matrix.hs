{-# LANGUAGE FlexibleContexts #-}
{- |

#TODO
  explain how to use lenses to manipulate fields in the matrix
-}
module Data.Matrix ( unMatrix
                   , showMatrix
                   , inMatrix
                   , transpose
                   , mul
                   , diagonal
                   , identity
                   , setDiagonal
                   , det
                   , inv
                   , gaussElim
                   , toMatrixList
                   , castContainer
                   , toMaybe
                   , whatever
                   ) where

import Control.Applicative
import Control.Monad.State hiding (sequence_, sequence)

import Data.Maybe
import Data.Foldable
import Data.Traversable
import Data.List hiding (mapAccumL, sum, product, concat, concatMap, transpose)
import qualified Data.List

import Data.Lenses
import Data.Vector

import Prelude hiding (sum, product, concat, concatMap, sequence_, sequence)


data Matrix row col a = Matrix (row (col a)) deriving (Show, Eq, Ord)

unMatrix :: Matrix row col a -> row (col a)
unMatrix (Matrix m) = m

showMatrix :: (Show a, Foldable row, Foldable col) => Matrix row col a -> String
showMatrix (Matrix m) = concatMap showRow $ toList m
  where
    showRow row = "| " ++ concat ((intersperse " ") (fmap show (toList row))) ++ " |\n"
--  | 1 2 |
--  | 2 3 |

inMatrix :: (t (f a) -> row (col b)) -> Matrix t f a -> Matrix row col b
inMatrix f (Matrix a) = Matrix (f a)

instance (Functor row, Functor col) => Functor (Matrix row col) where
  fmap f (Matrix a) = Matrix $ fmap (fmap f) a

instance (Monad row, Monad col, Applicative row, Applicative col, Traversable row, Traversable col) => Monad (Matrix row col) where
  return = Matrix . return . return
  (Matrix m) >>= f = Matrix $ m >>= sequenceA . join . fmap (sequenceA . unMatrix . f)
  --Matrix a <*> Matrix b = Matrix $ pure (<*>) <*> a <*> b

instance (Applicative row, Applicative col) => Applicative (Matrix row col) where
  -- # normally I would just use the monad instance functions 'return' and 'ap'
  -- # this is equivalent to the monad version, but doesn't require row and col to be instances of monad
  --pure = return
  --(<*>) = ap
  pure = Matrix . pure . pure
  Matrix a <*> Matrix b = Matrix $ pure (<*>) <*> a <*> b

instance (Foldable row, Foldable col) => Foldable (Matrix row col) where
  foldMap f (Matrix a) = foldMap (foldMap f) a

instance (Traversable row, Traversable col) => Traversable (Matrix row col) where
  traverse f (Matrix a) = Matrix <$> traverse (traverse f) a

instance (Num a, Applicative row, Applicative col, Eq (row (col a)), Show (row (col a))) => Num (Matrix row col a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Fractional a, Applicative row, Applicative col, Eq (row (col a)), Show (row (col a))) => Fractional (Matrix row col a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

-- | Transposes matrix
transpose :: (Traversable t, Applicative f) =>  Matrix t f a -> Matrix f t a
transpose m = inMatrix sequenceA m

-- | Matrix multiplication function
mul :: (Traversable f, Num a, Num (f a), Applicative row, Applicative col, Traversable col) =>
       Matrix row f a -> Matrix f col a -> Matrix row col a
mul (Matrix a) (Matrix b) = Matrix $ traverse (liftA2 dot a . pure) (sequenceA b) -- Oh yeah!

-- | Returns diagonal vector of matrix
diagonal :: (Monad f) => Matrix f f a -> f a
diagonal (Matrix a) = join a

-- | Returns the identity matrix (the size of the matrix is determined by the type)
identity :: (Num a, Traversable f, Monad f, Applicative f) => Matrix f f a
identity = setDiagonal 1 (pure 0)

-- | Set the diagonal of the matrix to a vector
setDiagonal :: (Traversable f, Monad f) => a -> Matrix f f a -> Matrix f f a
setDiagonal x m = (put x `to` diagonal) `from` m

-- | Calculates the determinate of a matrix (uses gaussian elimination, so it should scale well)
det :: (Fractional a, Foldable f, Functor f) => Matrix f f a -> a
det a = product $ zipWith id diagonalGets $ whatever $ gaussianElimination $ toMatrixList a

-- | Calculates the inverse of a matrix. Will return Nothing if the given matrix has no inverse.
--   (uses gaussian elimination, so it should scale well)
inv :: ( Fractional a,
         Traversable row, Monad row, Applicative row,
         Traversable col1, Applicative col1) =>
       Matrix row col1 a -> Maybe (Matrix row row a)
inv a = toMaybe $ fmap snd $ gaussElim a identity

-- | Performs gauss-jordan elimination on a matrix with an augmented matrix
gaussElim :: ( Traversable row, Applicative row,
        Traversable col1, Applicative col1,
        Traversable col2, Applicative col2,
        Fractional a ) =>
        Matrix row col1 a -> Matrix row col2 a -> GaussResult (Matrix row col1 a, Matrix row col2 a)
gaussElim a b = do
    --(primary, augmented) = gaussianElimination (toMatrixList a) (toMatrixList b)
    --a' = fromJust $ snd $ castContainer' (head $ head primary) (concat primary)
    -- b' = fromJust $ snd $ castContainer' (head $ head augmented) (concat augmented)
    result <- gaussianElimination $ zipWith (++) (toMatrixList a) (toMatrixList b)
    let big = concat $ Data.List.transpose $ reduceEchelon $ result
    let (xs, a') = mapSnd fromJust $ castContainer big
    let b' = fromJust $ snd $ castContainer xs
    return (transpose a', transpose b')

toMatrixList :: (Foldable row, Foldable col, Functor row) => Matrix row col a -> [[a]]
toMatrixList (Matrix a) = toList $ fmap toList a


castContainer :: (Foldable f, Traversable g, Applicative g, Num a) =>  f a -> ([a], Maybe (g a))
castContainer t = mapSnd sequenceA $ mapAccumL f (toList t) (pure ())
  where
    f [] _ = ([], Nothing)
    f (x:xs) _ = (xs, Just x)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

data GaussResult a = Solved a
                   | Underconstrained a
             deriving (Show, Eq)

toMaybe :: GaussResult a -> Maybe a
toMaybe (Solved x) = Just x
toMaybe (Underconstrained _) = Nothing

whatever :: GaussResult a -> a
whatever (Solved x) = x
whatever (Underconstrained x) = x

instance Functor GaussResult where
  fmap f (Solved x) = Solved (f x)
  fmap f (Underconstrained x) = Underconstrained (f x)

instance Monad GaussResult where
  return a = Solved a
  (Solved a) >>= f = f a
  (Underconstrained a) >>= f = Underconstrained $ whatever $ f a

instance Applicative GaussResult where
  pure = return
  (<*>) = ap

-- The gaussianElimination was graciously provided by Keith Sheppard
-- (with a few minor modifications...)
gaussianElimination :: (Fractional a) => [[a]] -> GaussResult [[a]]
gaussianElimination [] = Solved []
gaussianElimination [[]] = Solved [[]]
gaussianElimination coefMat = do -- yay for monads!
    -- work on zeroing out the leftmost coefficients in all equations
    -- below the top (ie below topCoefs). The ensureNonZeroFirstCoef
    -- function helps us deal with the special case where the first
    -- coefficient is zero
    topCoefs:otherCoefsMat <- ensureNonZeroFirstCoef coefMat
    let zeroOutFactors = map (zeroOutFactor topCoefs) otherCoefsMat

    let zeroOutCoefMatAddends = zipWith (map . (*)) zeroOutFactors (repeat topCoefs)
    let zeroedOutCoefMat = zipWith (zipWith (+)) otherCoefsMat zeroOutCoefMatAddends

    -- now that the leftmost coefs are zero we need to recurse into the
    -- lower right corner
    let lowerRightMat = map tail zeroedOutCoefMat
    newLowerRightMat <- gaussianElimination lowerRightMat

    -- OK now we just have to zip it up like we need
    let newLowerMat = zipWith (:) (map head zeroedOutCoefMat) newLowerRightMat
    return $ topCoefs:newLowerMat

-- this function is just to make sure that the left-most coefficient is non-zero. If the 1st leftmost coefficient is zero in the 1st row then that row is swapped with the 1st row that has a non-zero starting coefficient
ensureNonZeroFirstCoef :: (Num a) => [[a]] -> GaussResult [[a]]
ensureNonZeroFirstCoef coefMat =
  let firstNonZeroIndex = findIndex ((/= 0) . head) coefMat
  in case firstNonZeroIndex of
      Just 0  -> Solved $ coefMat
      Just i  -> Solved $ zeroSwap i coefMat
      Nothing -> Underconstrained coefMat -- error "Failed to find non-zero coefficient!"
  where zeroSwap i xs =
          let (start, finish) = splitAt i xs
          in head finish : start ++ tail finish

--the zeroOutFactor determines the factor that should be used to zero out the left-most coefficient in otherCoefs
zeroOutFactor :: (Fractional a) => [a] -> [a] -> a
zeroOutFactor topCoefs otherCoefs =
  let topLeftCoef = head topCoefs
      otherLeftCoef = negate $ head otherCoefs
  in if topLeftCoef /= 0 then otherLeftCoef / topLeftCoef else otherLeftCoef


diagonalGets :: [[a] -> a]
diagonalGets = iterate (. tail) head

reduceEchelon :: (Fractional a) => [[a]] -> [[a]]
reduceEchelon = reduceRows diagonalGets []

reduceRows :: (Fractional a) => [[a] -> a] -> [[a]] -> [[a]] -> [[a]]
reduceRows (f:fs) previousRows (row:rows) = reduceRows fs (reducedRows ++ [scaledRow]) rows
  where
    -- scale our row so diagonal is 1
    scaledRow = fmap (/ (f row)) row
    -- zero out the rest of that column
    reducedRows = fmap (reduce scaledRow) previousRows
    reduce sRow pRow = zipWith (-) pRow (fmap (* (f pRow)) sRow)
reduceRows _ previousRows _  = previousRows

{-
aTestMatrix :: (Num a) => Matrix Vector3 Vector3 a
aTestMatrix = Matrix (Vector3 (Vector3 0 1 3)
                              (Vector3 4 5 6)
                              (Vector3 7 8 9) )

-}
