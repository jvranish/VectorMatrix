module Data.Vector.Classes ( Size1(..)
                           , Size2(..)
                           , Size3(..)
                           , Size4(..)
                           , Size5(..)
                           , Size6(..)
                           , Size7(..)
                           , Size8(..)
                           , Size9(..)
                           , Size10(..)
                           , Size11(..)
                           , Size12(..)
                           , Size13(..)
                           , Size14(..)
                           , Size15(..)
                           , Size16(..)
                           , HasX(..)
                           , HasY(..)
                           , HasZ(..)
                           , HasT(..)
                           , HasU(..)
                           , HasV(..)
                           , HasW(..)
                           ) where

class Size1 f where
  packV1 :: a -> f a
  unpackV1 :: f a -> (a)

class Size2 f where
  packV2 :: a -> a -> f a
  unpackV2 :: f a -> (a, a)

class Size3 f where
  packV3 :: a -> a -> a -> f a
  unpackV3 :: f a -> (a, a, a)

class Size4 f where
  packV4 :: a -> a -> a -> a -> f a
  unpackV4 :: f a -> (a, a, a, a)

class Size5 f where
  packV5 :: a -> a -> a -> a -> a -> f a
  unpackV5 :: f a -> (a, a, a, a, a)

class Size6 f where
  packV6 :: a -> a -> a -> a -> a -> a -> f a
  unpackV6 :: f a -> (a, a, a, a, a, a)

class Size7 f where
  packV7 :: a -> a -> a -> a -> a -> a -> a -> f a
  unpackV7 :: f a -> (a, a, a, a, a, a, a)

class Size8 f where
  packV8 :: a -> a -> a -> a -> a -> a -> a -> a -> f a
  unpackV8 :: f a -> (a, a, a, a, a, a, a, a)

class Size9 f where
  packV9 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> f a
  unpackV9 :: f a -> (a, a, a, a, a, a, a, a, a)

class Size10 f where
  packV10 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> f a
  unpackV10 :: f a -> (a, a, a, a, a, a, a, a, a, a)

class Size11 f where
  packV11 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> f a
  unpackV11 :: f a -> (a, a, a, a, a, a, a, a, a, a, a)

class Size12 f where
  packV12 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> f a
  unpackV12 :: f a -> (a, a, a, a, a, a, a, a, a, a, a, a)

class Size13 f where
  packV13 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> f a
  unpackV13 :: f a -> (a, a, a, a, a, a, a, a, a, a, a, a, a)

class Size14 f where
  packV14 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> f a
  unpackV14 :: f a -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a)

class Size15 f where
  packV15 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> f a
  unpackV15 :: f a -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

class Size16 f where
  packV16 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> f a
  unpackV16 :: f a -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  
  
class HasX f where
  vx :: f a -> a
  uvx :: a -> f a -> f a

class HasY f where
  vy :: f a -> a
  uvy :: a -> f a -> f a

class HasZ f where
  vz :: f a -> a
  uvz :: a -> f a -> f a

class HasT f where
  vt :: f a -> a
  uvt :: a -> f a -> f a

class HasU f where
  vu :: f a -> a
  uvu :: a -> f a -> f a

class HasV f where
  vv :: f a -> a
  uvv :: a -> f a -> f a

class HasW f where
  vw :: f a -> a
  uvw :: a -> f a -> f a
  
