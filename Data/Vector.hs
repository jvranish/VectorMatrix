module Data.Vector ( dot
                   , mag
                   , normalize
                   , angleBetween
                   , scaleBy
                   , scaleTo
                   , direction
                   , distance
                   , projectOnto
                   , cross2
                   , signedAngle2
                   , positiveAngle2
                   , rotate2
                   , cross3
                   , module Data.Vector.Classes
                   , module Data.Vector.DataTypes
                   , module Data.Vector.Lenses
                   ) where

import Control.Applicative
import Data.Foldable 
import Data.Vector.DataTypes
import Data.Vector.Classes
import Data.Vector.Lenses
import Data.Vector.VectorInstances()
import Data.Vector.FixedListInstances()

import Prelude hiding (sum)

dot :: (Foldable f, Num a, Num (f a)) =>  f a -> f a -> a
dot a b = sum $ a * b

mag :: (Floating a, Foldable f, Num (f a)) => f a -> a
mag a = sqrt $ a `dot` a

normalize :: (Floating a, Foldable f, Applicative f, Fractional (f a)) => f a -> f a
normalize a = let m = mag a in if m /= 0 then a / pure m else a

angleBetween :: (Floating a, Foldable f, Num (f a)) =>  f a -> f a -> a
angleBetween a b = acos $ dot a b

scaleBy :: (Num a, Applicative f) => f a -> a -> f a
scaleBy a b = pure (b *) <*> a

scaleTo :: (Floating a, Foldable f, Num (f a), Applicative f) => f a -> a -> f a
scaleTo a b = a `scaleBy` (b / (mag a))

direction :: (Floating a, Foldable f, Applicative f, Fractional (f a)) => f a -> f a -> f a
direction a b = normalize $ b - a

distance :: (Floating a, Foldable f, Num (f a)) => f a -> f a -> a
distance a b = mag $ b - a

projectOnto :: (Foldable f, Fractional a, Applicative f, Num (f a)) => f a -> f a -> f a
projectOnto a b = b `scaleBy` ((a `dot` b)/(b `dot` b))

cross2 :: (Num t, Size2 f) => f t -> f t -> t
cross2 a b = (ax * by - ay * bx)
  where
    (ax, ay) = unpackV2 a
    (bx, by) = unpackV2 b

signedAngle2 :: (RealFloat a, Foldable f, Applicative f, Fractional (f a), Size2 f) => f a -> f a -> a
signedAngle2 a' b' = atan2 (a `cross2` b) (a `dot` b)
  where
    a = normalize a'
    b = normalize b'
    
positiveAngle2 :: (RealFloat a, Foldable f, Applicative f, Fractional (f a), Size2 f) =>  f a -> f a -> a
positiveAngle2 a b = if angle < 0 then 2 * pi + angle else angle
  where
    angle = signedAngle2 a b

rotate2 :: (Floating a, Size2 t, Size2 f) => f a -> a -> t a
rotate2 v angle = packV2 (x * c - y * s) (y * c + x * s)
  where
    c = cos angle
    s = sin angle
    (x, y) = unpackV2 v
    
-- | cross product is generalizable to N dimensions, but I'm not going to do it :(
-- well, maybe but it will be called perpendicular and will be in the Matrix module
cross3 :: (Size3 f, Size3 g, Size3 h, Num t) => f t -> g t -> h t
cross3 a b = packV3 (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)
  where
    (ax, ay, az) = unpackV3 a
    (bx, by, bz) = unpackV3 b


