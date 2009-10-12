{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE  UndecidableInstances #-}
{-# LANGUAGE  RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Matrix where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Data.List hiding (mapAccumL, sum, concat)

import Data.Lenses
import Data.ZipList hiding (diagonal)
import Data.Vector
import Control.Monad.State hiding (sequence_, sequence)

import Prelude hiding (sum, concat, sequence_, sequence)

data Matrix row col a = Matrix { unMatrix :: (row (col a))} deriving (Show, Eq, Ord)

inMatrix :: (t (f a) -> row (col b)) -> Matrix t f a -> Matrix row col b
inMatrix f (Matrix a) = Matrix (f a)

transpose :: (Traversable t, Applicative f) =>  Matrix t f a -> Matrix f t a
transpose m = inMatrix sequenceA m

mul :: (Traversable f, Num a, Num (f a), Applicative row, Applicative col, Traversable col) =>
       Matrix row f a -> Matrix f col a -> Matrix row col a
mul (Matrix a) (Matrix b) = Matrix $ traverse (liftA2 dot a . pure) (sequenceA b)

diagonal :: (Monad f) => Matrix f f a -> f a
diagonal (Matrix a) = join a

identity :: (Num a, Traversable f, Monad f, Applicative f) => Matrix f f a
identity = setDiagonal 1 (pure 0)

setDiagonal :: (Traversable f, Monad f) => a -> Matrix f f a -> Matrix f f a
setDiagonal x m = (put x `to` diagonal) `from` m

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


a = Matrix (Vector3 (Vector3 0 1 3)
                    (Vector3 4 5 6)
                    (Vector3 7 8 9) )
g 9 = Matrix (Vector3 (Vector3 0 0 0)
                      (Vector3 0 0 0)
                      (Vector3 0 0 1) )
g x = Matrix (Vector3 (Vector3 0 0 0)
                      (Vector3 0 0 0)
                      (Vector3 0 0 0) )
--  xs >>=
--m (m a) f
--pure f
{-

m a -> (a -> m b) -> m b
t (f a1) -> (a1 -> t (f a)) -> t (f a)


m (n a) -> (a -> m (n b)) -> m (n b)
n a -> (n a -> m (n b)) -> m (n b)
m >>= g

n a -> m (n b)
g x = sequenceA . join . fmap (sequenceA . f) x
sequenceA

m a -> (a -> m b) -> m b
>>=
>>=
a -> Matrix Vector2 Vector3 b
a -> Vector2 b
a -> Vector3 b

Matrix:
transpose
inverse
det
mul
scale
identity
diagonal
make transorm
 translate
 rotate
 scale
make projection
 ortho
 perspective

Vector
gradient
divergence
curl


Vector ()
-}


{-


OMG a very much work in progress 






-}






{-
zinits (ZipList xs) = ZipList $ ZipList <$> inits xs
ztails (ZipList xs) = ZipList $ ZipList <$> tails xs
ztail xs = inZ tail xs
zhead xs = fromInZ head xs
zcycle xs = inZ cycle xs
zcons x (ZipList xs) = ZipList (x:xs)
zappend (ZipList a) (ZipList b) = ZipList $ a ++ b
-}
{-
dropEachOnce :: Z a -> Z (Z a)
dropEachOnce xs = pure mappend <*> inits xs <*> (tail <$> tails xs)

negateOdds :: (Num a) => Z a -> Z a
negateOdds xs = (cycle (Z [id, negate])) <*> xs

det :: (Num a) => Z (Z a) -> a
det (Z []) = 1
det m  = sum $ negateOdds $ (head <$> m) * (det <$> dropEachOnce $ tail <$> m)

diagonalGets = iterate (. tail) head

diagonal :: (Num a) => Z a -> Z (Z a)
diagonal xs = pure mappend <*> inits zeroed <*> (pure cons <*> xs <*> (tail $ tails zeroed))
  where
    zeroed = fmap (const 0) xs

getDiagonal xs = diagonalGets <*> xs -- Oh if only moand and applicative instances for List were done properly...

test :: (Num a) => Z (Z a)
test = Z $ Z <$> [ [ 2, -1,  0, 1, 0, 0 ]
                 , [-1,  2, -1, 0, 1, 0 ]
                 , [ 0, -1,  2, 0, 0, 1 ]
                 ]
-}

--sum $ zipWith (+) (negateOdds xs) (tail xs) == 0
--negateOdds $ negateOdds xs = xs
-- getDiagonal $ diagonal xs == xs
--product $ getDiagonal $ gauseElim xs == det xs
-- a `mul` identity = a
-- det (a `mul` b) == det (b `mul` a)
--a `mul` (b + c) == a `mul` b + a `mul` c
--(a + b) `mul` c == a `mul` c + b `mul` c
-- inverse a `mul` a = identity

--
--gaussElim xs =
--find pivot n
--swap with row n
--multiply following rows by correct factor

-- The gaussianElimination was graciously provided by Keith Sheppard
-- (with a few minor modifications...)
{-
gaussianElimination :: (Fractional a) => [[a]] -> [a] -> ([[a]], [a])
gaussianElimination [] [] = ([], [])
gaussianElimination [[]] x = ([[]], x)
--gaussianElimination [] _ = error "Matrix row count is greater than right hand size!"
--gaussianElimination _ [] = error "Matrix row count is less than right hand size!"
gaussianElimination coefMat rhsVec =
  let
      -- work on zeroing out the leftmost coefficients in all equations
      -- below the top (ie below topCoefs). The ensureNonZeroFirstCoef
      -- function helps us deal with the special case where the first
      -- coefficient is zero
      (topCoefs:otherCoefsMat, topRHSVal:otherRHSVals) =
          ensureNonZeroFirstCoef coefMat rhsVec
      zeroOutFactors = map (zeroOutFactor topCoefs) otherCoefsMat

      zeroOutCoefMatAddends = zipWith (map . (*)) zeroOutFactors (repeat topCoefs)
      zeroedOutCoefMat = zipWith (zipWith (+)) otherCoefsMat zeroOutCoefMatAddends

      -- now calculate the right-hand-side to go along with the
      -- zeroed out coefficients
      zeroOutRHSAddends = map (topRHSVal *) zeroOutFactors
      zeroedOutRHS = zipWith (+) otherRHSVals zeroOutRHSAddends

      -- now that the leftmost coefs are zero we need to recurse into the
      -- lower right corner
      lowerRightMat = map tail zeroedOutCoefMat
      (newLowerRightMat, newLowerRHS) = gaussianElimination lowerRightMat zeroedOutRHS

      -- OK now we just have to zip it up like we need
      newLowerMat = zipWith (:) (map head zeroedOutCoefMat) newLowerRightMat
      finalMat = topCoefs:newLowerMat
      finalRHS = topRHSVal:newLowerRHS
  in
      (finalMat, finalRHS)

-- this function is just to make sure that the left-most coefficient is non-zero. If the 1st leftmost coefficient is zero in the 1st row then that row is swapped with the 1st row that has a non-zero starting coefficient

ensureNonZeroFirstCoef coefMat rhsVec =
  let firstNonZeroIndex = findIndex ((/= 0) . head) coefMat
  in case firstNonZeroIndex of
      Just 0  -> (coefMat, rhsVec)
      Just i  -> (zeroSwap i coefMat, zeroSwap i rhsVec)
      Nothing -> (coefMat, rhsVec) -- error "Failed to find non-zero coefficient!"
  where zeroSwap i xs =
          let (start, finish) = splitAt i xs
          in head finish : start ++ tail finish

--the zeroOutFactor determines the factor that should be used to zero out the left-most coefficient in otherCoefs

zeroOutFactor :: (Fractional a) => [a] -> [a] -> a
zeroOutFactor topCoefs otherCoefs =
  let topLeftCoef = head topCoefs
      otherLeftCoef = negate $ head otherCoefs
  in if topLeftCoef /= 0 then otherLeftCoef / topLeftCoef else otherLeftCoef


getUppers xs = zipWith fmap diagonalGets (tail $ inits xs)

reduceEchelon = reduceRows diagonalGets []

reduceRows _ previousRows []  = previousRows
reduceRows (f:fs) previousRows (row:rows) = reduceRows fs (reducedRows ++ [scaledRow]) rows
  where
    -- scale our row so diagonal is 1
    scaledRow = fmap (/ (f row)) row
    -- zero out the rest of that column
    reducedRows = fmap (reduce scaledRow) previousRows
    reduce sRow pRow = zipWith (-) pRow (fmap (* (f pRow)) sRow)

showMatrix xs = (concat $ intersperse "\n" $ fmap show xs ) ++ "\n"
-}
-- puts into reduced echelon form
-- we return a Maybe here because what I really want this for is
{-
check if diagonal has zero, return Nothing
otherwise
reduceEchelon
(0:_) = Nothing
(x:xs) = Just $
getDiagonal xs
start at the bottom
divide by diagonal and add to remaining rows

asdf diagonal rows



asdf (x:xs) (row:rows) completedRows
  scaledRow = fmap (/x) row
  zipWith (fmap) (tail diagonalGets) completedRows
  getDiagonal $ fmap tail completedRows
  reduceRow currentRow x previousRow = zipWith (-) previousRow (fmap (*x) currentRow)
  zipWith (reduceRow scaledRow)
  getUppers

-}
{-
mapMatrix ::
mapMatrix f (Matrix a) = Matrix $ f a

-- That you can do this with traverse and applicative still blows my mind
transpose :: (Traversable col, Applicative row) => Matrix col row e -> Matrix row col e
transpose (Matrix a) = Matrix $ sequenceA a

-- And this blows it even more!
mul :: (Traversable t, Traversable col,
        Applicative t, Applicative row,Applicative col,
        Num a, Show (t a), Eq (t a) ) =>
       Matrix row t a -> Matrix t col a -> Matrix row col a
mul (Matrix a) (Matrix b) = Matrix $ traverse (liftA2 dot a . pure) (sequenceA b)

--prependZero xs = 0:xs
identityList :: (Num a) => [[a]]
identityList = iterate (0:) $ 1 : repeat 0
identity :: (Applicative col, Applicative row,
             Traversable col, Traversable row,
             Num a ) =>
            Matrix row col a
identity = Matrix $ superMap (superMap id) identityList
-- snd $ mapAccumL ( \(x:xs) a -> (xs, x) ) identityList (pure undefined)
--identity = snd $ mapAccumL ( \(x:xs) a -> (xs, x) ) identityList (pure undefined)
-}
--note this will crash if g is bigger than f
superMap :: (Foldable f, Applicative g, Traversable g) => (a -> b) -> f a -> g b
superMap f t = snd $ mapAccumL ( \(x:xs) a -> (xs, f x) ) (toList t) (pure undefined)

castContainer :: (Foldable f, Applicative g, Traversable g) => f a -> Maybe (g a)
castContainer t = sequenceA $ snd $ mapAccumL f (toList t) (pure undefined)
  where
    f [] _ = ([], Nothing)
    f (x:xs) _ = (xs, Just x)

{-

matrix

transpose
inverse
det
mul
scale
identity
diagonal
make transorm
 translate
 rotate
 scale
make projection
 ortho
 perspective


superMap :: (Foldable f, Applicative g, Traversable g) => (a -> b) -> f a -> g b
superMap f t = snd $ mapAccumL ( \(x:xs) a -> (xs, f x) ) (toList t) t


replaceElements :: (Traversable f) => [a] -> f b -> f a
replaceElements xs t = snd $ mapAccumL ( \(x:xs) a -> (xs, x) ) xs t

replaceIndex n x xs = (take n xs) ++ [x] ++ (drop (n + 1) xs)
f n a x = (replaceElements (replaceIndex n a $ toList x) x
getLenses t = replaceElements (fmap f [0..]) t
-}

{-
instance (Applicative row, Applicative col) => Applicative (Matrix row col) where
  pure = Matrix . pure . pure
  Matrix a <*> Matrix b = Matrix $ pure (<*>) <*> a <*> b

instance (Foldable row, Foldable col) => Foldable (Matrix row col) where
  foldMap f (Matrix a) = foldMap (foldMap f) a

instance (Traversable row, Traversable col) => Traversable (Matrix row col) where
  traverse f (Matrix a) = Matrix <$> traverse (traverse f) a
  -}

