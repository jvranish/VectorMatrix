{-# OPTIONS -fno-warn-orphans #-}
module Data.Vector.VectorInstances where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Monoid

import Data.Vector.Classes
import Data.Vector.DataTypes

instance Functor Vector1 where
  fmap f' (Vector1 a) = Vector1 (f' a)
instance Functor Vector2 where
  fmap f' (Vector2 a b) = Vector2 (f' a) (f' b)
instance Functor Vector3 where
  fmap f' (Vector3 a b c) = Vector3 (f' a) (f' b) (f' c)
instance Functor Vector4 where
  fmap f' (Vector4 a b c d) = Vector4 (f' a) (f' b) (f' c) (f' d)
instance Functor Vector5 where
  fmap f' (Vector5 a b c d e) = Vector5 (f' a) (f' b) (f' c) (f' d) (f' e)
instance Functor Vector6 where
  fmap f' (Vector6 a b c d e f) = Vector6 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f)
instance Functor Vector7 where
  fmap f' (Vector7 a b c d e f g) = Vector7 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g)
instance Functor Vector8 where
  fmap f' (Vector8 a b c d e f g h) = Vector8 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h)
instance Functor Vector9 where
  fmap f' (Vector9 a b c d e f g h i) = Vector9 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i)
instance Functor Vector10 where
  fmap f' (Vector10 a b c d e f g h i j) = Vector10 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j)
instance Functor Vector11 where
  fmap f' (Vector11 a b c d e f g h i j k) = Vector11 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k)
instance Functor Vector12 where
  fmap f' (Vector12 a b c d e f g h i j k l) = Vector12 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k) (f' l)
instance Functor Vector13 where
  fmap f' (Vector13 a b c d e f g h i j k l m) = Vector13 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k) (f' l) (f' m)
instance Functor Vector14 where
  fmap f' (Vector14 a b c d e f g h i j k l m n) = Vector14 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k) (f' l) (f' m) (f' n)
instance Functor Vector15 where
  fmap f' (Vector15 a b c d e f g h i j k l m n o) = Vector15 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k) (f' l) (f' m) (f' n) (f' o)
instance Functor Vector16 where
  fmap f' (Vector16 a b c d e f g h i j k l m n o p) = Vector16 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k) (f' l) (f' m) (f' n) (f' o) (f' p)
  
instance Monad Vector1 where
  return a = Vector1 a
  Vector1 a >>= f' = case Vector1 (f' a) of
    Vector1 (Vector1 b) -> Vector1 b
instance Monad Vector2 where
  return a = Vector2 a a
  Vector2 a b >>= f' = case Vector2 (f' a) (f' b) of
    Vector2 (Vector2 c _) (Vector2 _ d) -> Vector2 c d
instance Monad Vector3 where
  return a = Vector3 a a a
  Vector3 a b c >>= f' = case Vector3 (f' a) (f' b) (f' c) of
    Vector3 (Vector3 d _ _) (Vector3 _ e _) (Vector3 _ _ f) -> Vector3 d e f
instance Monad Vector4 where
  return a = Vector4 a a a a
  Vector4 a b c d >>= f' = case Vector4 (f' a) (f' b) (f' c) (f' d) of
    Vector4 (Vector4 e _ _ _) (Vector4 _ f _ _) (Vector4 _ _ g _) (Vector4 _ _ _ h) -> Vector4 e f g h
instance Monad Vector5 where
  return a = Vector5 a a a a a
  Vector5 a b c d e >>= f' = case Vector5 (f' a) (f' b) (f' c) (f' d) (f' e) of
    Vector5 (Vector5 f _ _ _ _) (Vector5 _ g _ _ _) (Vector5 _ _ h _ _) (Vector5 _ _ _ i _) (Vector5 _ _ _ _ j) -> Vector5 f g h i j
instance Monad Vector6 where
  return a = Vector6 a a a a a a
  Vector6 a b c d e f >>= f' = case Vector6 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) of
    Vector6 (Vector6 g _ _ _ _ _) (Vector6 _ h _ _ _ _) (Vector6 _ _ i _ _ _) (Vector6 _ _ _ j _ _) (Vector6 _ _ _ _ k _) (Vector6 _ _ _ _ _ l) -> Vector6 g h i j k l
instance Monad Vector7 where
  return a = Vector7 a a a a a a a
  Vector7 a b c d e f g >>= f' = case Vector7 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) of
    Vector7 (Vector7 h _ _ _ _ _ _) (Vector7 _ i _ _ _ _ _) (Vector7 _ _ j _ _ _ _) (Vector7 _ _ _ k _ _ _) (Vector7 _ _ _ _ l _ _) (Vector7 _ _ _ _ _ m _) (Vector7 _ _ _ _ _ _ n) -> Vector7 h i j k l m n
instance Monad Vector8 where
  return a = Vector8 a a a a a a a a
  Vector8 a b c d e f g h >>= f' = case Vector8 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) of
    Vector8 (Vector8 i _ _ _ _ _ _ _) (Vector8 _ j _ _ _ _ _ _) (Vector8 _ _ k _ _ _ _ _) (Vector8 _ _ _ l _ _ _ _) (Vector8 _ _ _ _ m _ _ _) (Vector8 _ _ _ _ _ n _ _) (Vector8 _ _ _ _ _ _ o _) (Vector8 _ _ _ _ _ _ _ p) -> Vector8 i j k l m n o p
instance Monad Vector9 where
  return a = Vector9 a a a a a a a a a
  Vector9 a b c d e f g h i >>= f' = case Vector9 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) of
    Vector9 (Vector9 j _ _ _ _ _ _ _ _) (Vector9 _ k _ _ _ _ _ _ _) (Vector9 _ _ l _ _ _ _ _ _) (Vector9 _ _ _ m _ _ _ _ _) (Vector9 _ _ _ _ n _ _ _ _) (Vector9 _ _ _ _ _ o _ _ _) (Vector9 _ _ _ _ _ _ p _ _) (Vector9 _ _ _ _ _ _ _ q _) (Vector9 _ _ _ _ _ _ _ _ r) -> Vector9 j k l m n o p q r
instance Monad Vector10 where
  return a = Vector10 a a a a a a a a a a
  Vector10 a b c d e f g h i j >>= f' = case Vector10 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) of
    Vector10 (Vector10 k _ _ _ _ _ _ _ _ _) (Vector10 _ l _ _ _ _ _ _ _ _) (Vector10 _ _ m _ _ _ _ _ _ _) (Vector10 _ _ _ n _ _ _ _ _ _) (Vector10 _ _ _ _ o _ _ _ _ _) (Vector10 _ _ _ _ _ p _ _ _ _) (Vector10 _ _ _ _ _ _ q _ _ _) (Vector10 _ _ _ _ _ _ _ r _ _) (Vector10 _ _ _ _ _ _ _ _ s _) (Vector10 _ _ _ _ _ _ _ _ _ t) -> Vector10 k l m n o p q r s t
instance Monad Vector11 where
  return a = Vector11 a a a a a a a a a a a
  Vector11 a b c d e f g h i j k >>= f' = case Vector11 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k) of
    Vector11 (Vector11 l _ _ _ _ _ _ _ _ _ _) (Vector11 _ m _ _ _ _ _ _ _ _ _) (Vector11 _ _ n _ _ _ _ _ _ _ _) (Vector11 _ _ _ o _ _ _ _ _ _ _) (Vector11 _ _ _ _ p _ _ _ _ _ _) (Vector11 _ _ _ _ _ q _ _ _ _ _) (Vector11 _ _ _ _ _ _ r _ _ _ _) (Vector11 _ _ _ _ _ _ _ s _ _ _) (Vector11 _ _ _ _ _ _ _ _ t _ _) (Vector11 _ _ _ _ _ _ _ _ _ u _) (Vector11 _ _ _ _ _ _ _ _ _ _ v) -> Vector11 l m n o p q r s t u v
instance Monad Vector12 where
  return a = Vector12 a a a a a a a a a a a a
  Vector12 a b c d e f g h i j k l >>= f' = case Vector12 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k) (f' l) of
    Vector12 (Vector12 m _ _ _ _ _ _ _ _ _ _ _) (Vector12 _ n _ _ _ _ _ _ _ _ _ _) (Vector12 _ _ o _ _ _ _ _ _ _ _ _) (Vector12 _ _ _ p _ _ _ _ _ _ _ _) (Vector12 _ _ _ _ q _ _ _ _ _ _ _) (Vector12 _ _ _ _ _ r _ _ _ _ _ _) (Vector12 _ _ _ _ _ _ s _ _ _ _ _) (Vector12 _ _ _ _ _ _ _ t _ _ _ _) (Vector12 _ _ _ _ _ _ _ _ u _ _ _) (Vector12 _ _ _ _ _ _ _ _ _ v _ _) (Vector12 _ _ _ _ _ _ _ _ _ _ w _) (Vector12 _ _ _ _ _ _ _ _ _ _ _ x) -> Vector12 m n o p q r s t u v w x
instance Monad Vector13 where
  return a = Vector13 a a a a a a a a a a a a a
  Vector13 a b c d e f g h i j k l m >>= f' = case Vector13 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k) (f' l) (f' m) of
    Vector13 (Vector13 n _ _ _ _ _ _ _ _ _ _ _ _) (Vector13 _ o _ _ _ _ _ _ _ _ _ _ _) (Vector13 _ _ p _ _ _ _ _ _ _ _ _ _) (Vector13 _ _ _ q _ _ _ _ _ _ _ _ _) (Vector13 _ _ _ _ r _ _ _ _ _ _ _ _) (Vector13 _ _ _ _ _ s _ _ _ _ _ _ _) (Vector13 _ _ _ _ _ _ t _ _ _ _ _ _) (Vector13 _ _ _ _ _ _ _ u _ _ _ _ _) (Vector13 _ _ _ _ _ _ _ _ v _ _ _ _) (Vector13 _ _ _ _ _ _ _ _ _ w _ _ _) (Vector13 _ _ _ _ _ _ _ _ _ _ x _ _) (Vector13 _ _ _ _ _ _ _ _ _ _ _ y _) (Vector13 _ _ _ _ _ _ _ _ _ _ _ _ z) -> Vector13 n o p q r s t u v w x y z
instance Monad Vector14 where
  return a = Vector14 a a a a a a a a a a a a a a
  Vector14 a b c d e f g h i j k l m n >>= f' = case Vector14 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k) (f' l) (f' m) (f' n) of
    Vector14 (Vector14 o _ _ _ _ _ _ _ _ _ _ _ _ _) (Vector14 _ p _ _ _ _ _ _ _ _ _ _ _ _) (Vector14 _ _ q _ _ _ _ _ _ _ _ _ _ _) (Vector14 _ _ _ r _ _ _ _ _ _ _ _ _ _) (Vector14 _ _ _ _ s _ _ _ _ _ _ _ _ _) (Vector14 _ _ _ _ _ t _ _ _ _ _ _ _ _) (Vector14 _ _ _ _ _ _ u _ _ _ _ _ _ _) (Vector14 _ _ _ _ _ _ _ v _ _ _ _ _ _) (Vector14 _ _ _ _ _ _ _ _ w _ _ _ _ _) (Vector14 _ _ _ _ _ _ _ _ _ x _ _ _ _) (Vector14 _ _ _ _ _ _ _ _ _ _ y _ _ _) (Vector14 _ _ _ _ _ _ _ _ _ _ _ z _ _) (Vector14 _ _ _ _ _ _ _ _ _ _ _ _ a1 _) (Vector14 _ _ _ _ _ _ _ _ _ _ _ _ _ b1) -> Vector14 o p q r s t u v w x y z a1 b1
instance Monad Vector15 where
  return a = Vector15 a a a a a a a a a a a a a a a
  Vector15 a b c d e f g h i j k l m n o >>= f' = case Vector15 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k) (f' l) (f' m) (f' n) (f' o) of
    Vector15 (Vector15 p _ _ _ _ _ _ _ _ _ _ _ _ _ _) (Vector15 _ q _ _ _ _ _ _ _ _ _ _ _ _ _) (Vector15 _ _ r _ _ _ _ _ _ _ _ _ _ _ _) (Vector15 _ _ _ s _ _ _ _ _ _ _ _ _ _ _) (Vector15 _ _ _ _ t _ _ _ _ _ _ _ _ _ _) (Vector15 _ _ _ _ _ u _ _ _ _ _ _ _ _ _) (Vector15 _ _ _ _ _ _ v _ _ _ _ _ _ _ _) (Vector15 _ _ _ _ _ _ _ w _ _ _ _ _ _ _) (Vector15 _ _ _ _ _ _ _ _ x _ _ _ _ _ _) (Vector15 _ _ _ _ _ _ _ _ _ y _ _ _ _ _) (Vector15 _ _ _ _ _ _ _ _ _ _ z _ _ _ _) (Vector15 _ _ _ _ _ _ _ _ _ _ _ a1 _ _ _) (Vector15 _ _ _ _ _ _ _ _ _ _ _ _ b1 _ _) (Vector15 _ _ _ _ _ _ _ _ _ _ _ _ _ c1 _) (Vector15 _ _ _ _ _ _ _ _ _ _ _ _ _ _ d1) -> Vector15 p q r s t u v w x y z a1 b1 c1 d1
instance Monad Vector16 where
  return a = Vector16 a a a a a a a a a a a a a a a a
  Vector16 a b c d e f g h i j k l m n o p >>= f' = case Vector16 (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k) (f' l) (f' m) (f' n) (f' o) (f' p) of
    Vector16 (Vector16 q _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) (Vector16 _ r _ _ _ _ _ _ _ _ _ _ _ _ _ _) (Vector16 _ _ s _ _ _ _ _ _ _ _ _ _ _ _ _) (Vector16 _ _ _ t _ _ _ _ _ _ _ _ _ _ _ _) (Vector16 _ _ _ _ u _ _ _ _ _ _ _ _ _ _ _) (Vector16 _ _ _ _ _ v _ _ _ _ _ _ _ _ _ _) (Vector16 _ _ _ _ _ _ w _ _ _ _ _ _ _ _ _) (Vector16 _ _ _ _ _ _ _ x _ _ _ _ _ _ _ _) (Vector16 _ _ _ _ _ _ _ _ y _ _ _ _ _ _ _) (Vector16 _ _ _ _ _ _ _ _ _ z _ _ _ _ _ _) (Vector16 _ _ _ _ _ _ _ _ _ _ a1 _ _ _ _ _) (Vector16 _ _ _ _ _ _ _ _ _ _ _ b1 _ _ _ _) (Vector16 _ _ _ _ _ _ _ _ _ _ _ _ c1 _ _ _) (Vector16 _ _ _ _ _ _ _ _ _ _ _ _ _ d1 _ _) (Vector16 _ _ _ _ _ _ _ _ _ _ _ _ _ _ e1 _) (Vector16 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ f1) -> Vector16 q r s t u v w x y z a1 b1 c1 d1 e1 f1

instance Applicative Vector1 where
  pure = return
  (<*>) = ap
instance Applicative Vector2 where
  pure = return
  (<*>) = ap
instance Applicative Vector3 where
  pure = return
  (<*>) = ap
instance Applicative Vector4 where
  pure = return
  (<*>) = ap
instance Applicative Vector5 where
  pure = return
  (<*>) = ap
instance Applicative Vector6 where
  pure = return
  (<*>) = ap
instance Applicative Vector7 where
  pure = return
  (<*>) = ap
instance Applicative Vector8 where
  pure = return
  (<*>) = ap
instance Applicative Vector9 where
  pure = return
  (<*>) = ap
instance Applicative Vector10 where
  pure = return
  (<*>) = ap
instance Applicative Vector11 where
  pure = return
  (<*>) = ap
instance Applicative Vector12 where
  pure = return
  (<*>) = ap
instance Applicative Vector13 where
  pure = return
  (<*>) = ap
instance Applicative Vector14 where
  pure = return
  (<*>) = ap
instance Applicative Vector15 where
  pure = return
  (<*>) = ap
instance Applicative Vector16 where
  pure = return
  (<*>) = ap

instance Foldable Vector1 where
  foldMap f' (Vector1 a) = f' a
instance Foldable Vector2 where
  foldMap f' (Vector2 a b) = f' a `mappend` f' b
instance Foldable Vector3 where
  foldMap f' (Vector3 a b c) = f' a `mappend` f' b `mappend` f' c
instance Foldable Vector4 where
  foldMap f' (Vector4 a b c d) = f' a `mappend` f' b `mappend` f' c `mappend` f' d
instance Foldable Vector5 where
  foldMap f' (Vector5 a b c d e) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e
instance Foldable Vector6 where
  foldMap f' (Vector6 a b c d e f) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e `mappend` f' f
instance Foldable Vector7 where
  foldMap f' (Vector7 a b c d e f g) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e `mappend` f' f `mappend` f' g
instance Foldable Vector8 where
  foldMap f' (Vector8 a b c d e f g h) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e `mappend` f' f `mappend` f' g `mappend` f' h
instance Foldable Vector9 where
  foldMap f' (Vector9 a b c d e f g h i) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e `mappend` f' f `mappend` f' g `mappend` f' h `mappend` f' i
instance Foldable Vector10 where
  foldMap f' (Vector10 a b c d e f g h i j) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e `mappend` f' f `mappend` f' g `mappend` f' h `mappend` f' i `mappend` f' j
instance Foldable Vector11 where
  foldMap f' (Vector11 a b c d e f g h i j k) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e `mappend` f' f `mappend` f' g `mappend` f' h `mappend` f' i `mappend` f' j `mappend` f' k
instance Foldable Vector12 where
  foldMap f' (Vector12 a b c d e f g h i j k l) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e `mappend` f' f `mappend` f' g `mappend` f' h `mappend` f' i `mappend` f' j `mappend` f' k `mappend` f' l
instance Foldable Vector13 where
  foldMap f' (Vector13 a b c d e f g h i j k l m) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e `mappend` f' f `mappend` f' g `mappend` f' h `mappend` f' i `mappend` f' j `mappend` f' k `mappend` f' l `mappend` f' m
instance Foldable Vector14 where
  foldMap f' (Vector14 a b c d e f g h i j k l m n) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e `mappend` f' f `mappend` f' g `mappend` f' h `mappend` f' i `mappend` f' j `mappend` f' k `mappend` f' l `mappend` f' m `mappend` f' n
instance Foldable Vector15 where
  foldMap f' (Vector15 a b c d e f g h i j k l m n o) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e `mappend` f' f `mappend` f' g `mappend` f' h `mappend` f' i `mappend` f' j `mappend` f' k `mappend` f' l `mappend` f' m `mappend` f' n `mappend` f' o
instance Foldable Vector16 where
  foldMap f' (Vector16 a b c d e f g h i j k l m n o p) = f' a `mappend` f' b `mappend` f' c `mappend` f' d `mappend` f' e `mappend` f' f `mappend` f' g `mappend` f' h `mappend` f' i `mappend` f' j `mappend` f' k `mappend` f' l `mappend` f' m `mappend` f' n `mappend` f' o `mappend` f' p
  
instance Traversable Vector1 where
  traverse f' (Vector1 a) = Vector1 <$> f' a
instance Traversable Vector2 where
  traverse f' (Vector2 a b) = Vector2 <$> f' a <*> f' b
instance Traversable Vector3 where
  traverse f' (Vector3 a b c) = Vector3 <$> f' a <*> f' b <*> f' c
instance Traversable Vector4 where
  traverse f' (Vector4 a b c d) = Vector4 <$> f' a <*> f' b <*> f' c <*> f' d
instance Traversable Vector5 where
  traverse f' (Vector5 a b c d e) = Vector5 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e
instance Traversable Vector6 where
  traverse f' (Vector6 a b c d e f) = Vector6 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e <*> f' f
instance Traversable Vector7 where
  traverse f' (Vector7 a b c d e f g) = Vector7 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e <*> f' f <*> f' g
instance Traversable Vector8 where
  traverse f' (Vector8 a b c d e f g h) = Vector8 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e <*> f' f <*> f' g <*> f' h
instance Traversable Vector9 where
  traverse f' (Vector9 a b c d e f g h i) = Vector9 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e <*> f' f <*> f' g <*> f' h <*> f' i
instance Traversable Vector10 where
  traverse f' (Vector10 a b c d e f g h i j) = Vector10 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e <*> f' f <*> f' g <*> f' h <*> f' i <*> f' j
instance Traversable Vector11 where
  traverse f' (Vector11 a b c d e f g h i j k) = Vector11 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e <*> f' f <*> f' g <*> f' h <*> f' i <*> f' j <*> f' k
instance Traversable Vector12 where
  traverse f' (Vector12 a b c d e f g h i j k l) = Vector12 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e <*> f' f <*> f' g <*> f' h <*> f' i <*> f' j <*> f' k <*> f' l
instance Traversable Vector13 where
  traverse f' (Vector13 a b c d e f g h i j k l m) = Vector13 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e <*> f' f <*> f' g <*> f' h <*> f' i <*> f' j <*> f' k <*> f' l <*> f' m
instance Traversable Vector14 where
  traverse f' (Vector14 a b c d e f g h i j k l m n) = Vector14 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e <*> f' f <*> f' g <*> f' h <*> f' i <*> f' j <*> f' k <*> f' l <*> f' m <*> f' n
instance Traversable Vector15 where
  traverse f' (Vector15 a b c d e f g h i j k l m n o) = Vector15 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e <*> f' f <*> f' g <*> f' h <*> f' i <*> f' j <*> f' k <*> f' l <*> f' m <*> f' n <*> f' o
instance Traversable Vector16 where
  traverse f' (Vector16 a b c d e f g h i j k l m n o p) = Vector16 <$> f' a <*> f' b <*> f' c <*> f' d <*> f' e <*> f' f <*> f' g <*> f' h <*> f' i <*> f' j <*> f' k <*> f' l <*> f' m <*> f' n <*> f' o <*> f' p
  
instance (Num a) => Num (Vector1 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector2 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector3 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector4 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector5 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector6 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector7 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector8 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector9 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector10 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector11 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector12 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector13 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector14 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector15 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector16 a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Vector1 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector2 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector3 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector4 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector5 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector6 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector7 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector8 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector9 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector10 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector11 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector12 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector13 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector14 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector15 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector16 a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational
  

instance Size1 Vector1 where
  packV1 a = Vector1 a
  unpackV1 (Vector1 a) = (a)

instance Size2 Vector2 where
  packV2 a b = Vector2 a b
  unpackV2 (Vector2 a b) = (a, b)

instance Size3 Vector3 where
  packV3 a b c = Vector3 a b c
  unpackV3 (Vector3 a b c) = (a, b, c)

instance Size4 Vector4 where
  packV4 a b c d = Vector4 a b c d
  unpackV4 (Vector4 a b c d) = (a, b, c, d)

instance Size5 Vector5 where
  packV5 a b c d e = Vector5 a b c d e
  unpackV5 (Vector5 a b c d e) = (a, b, c, d, e)

instance Size6 Vector6 where
  packV6 a b c d e f = Vector6 a b c d e f
  unpackV6 (Vector6 a b c d e f) = (a, b, c, d, e, f)

instance Size7 Vector7 where
  packV7 a b c d e f g = Vector7 a b c d e f g
  unpackV7 (Vector7 a b c d e f g) = (a, b, c, d, e, f, g)

instance Size8 Vector8 where
  packV8 a b c d e f g h = Vector8 a b c d e f g h
  unpackV8 (Vector8 a b c d e f g h) = (a, b, c, d, e, f, g, h)

instance Size9 Vector9 where
  packV9 a b c d e f g h i = Vector9 a b c d e f g h i
  unpackV9 (Vector9 a b c d e f g h i) = (a, b, c, d, e, f, g, h, i)

instance Size10 Vector10 where
  packV10 a b c d e f g h i j = Vector10 a b c d e f g h i j
  unpackV10 (Vector10 a b c d e f g h i j) = (a, b, c, d, e, f, g, h, i, j)

instance Size11 Vector11 where
  packV11 a b c d e f g h i j k = Vector11 a b c d e f g h i j k
  unpackV11 (Vector11 a b c d e f g h i j k) = (a, b, c, d, e, f, g, h, i, j, k)

instance Size12 Vector12 where
  packV12 a b c d e f g h i j k l = Vector12 a b c d e f g h i j k l
  unpackV12 (Vector12 a b c d e f g h i j k l) = (a, b, c, d, e, f, g, h, i, j, k, l)

instance Size13 Vector13 where
  packV13 a b c d e f g h i j k l m = Vector13 a b c d e f g h i j k l m
  unpackV13 (Vector13 a b c d e f g h i j k l m) = (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance Size14 Vector14 where
  packV14 a b c d e f g h i j k l m n = Vector14 a b c d e f g h i j k l m n
  unpackV14 (Vector14 a b c d e f g h i j k l m n) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance Size15 Vector15 where
  packV15 a b c d e f g h i j k l m n o = Vector15 a b c d e f g h i j k l m n o
  unpackV15 (Vector15 a b c d e f g h i j k l m n o) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance Size16 Vector16 where
  packV16 a b c d e f g h i j k l m n o p = Vector16 a b c d e f g h i j k l m n o p
  unpackV16 (Vector16 a b c d e f g h i j k l m n o p) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)


instance HasX Vector1 where
  vx (Vector1 a) = a
  uvx a' (Vector1 _) = (Vector1 a')

instance HasX Vector2 where
  vx (Vector2 a _) = a
  uvx a' (Vector2 _ b) = (Vector2 a' b)

instance HasY Vector2 where
  vy (Vector2 _ b) = b
  uvy b' (Vector2 a _) = (Vector2 a b')

instance HasX Vector3 where
  vx (Vector3 a _ _) = a
  uvx a' (Vector3 _ b c) = (Vector3 a' b c)

instance HasY Vector3 where
  vy (Vector3 _ b _) = b
  uvy b' (Vector3 a _ c) = (Vector3 a b' c)

instance HasZ Vector3 where
  vz (Vector3 _ _ c) = c
  uvz c' (Vector3 a b _) = (Vector3 a b c')

instance HasX Vector4 where
  vx (Vector4 a _ _ _) = a
  uvx a' (Vector4 _ b c d) = (Vector4 a' b c d)

instance HasY Vector4 where
  vy (Vector4 _ b _ _) = b
  uvy b' (Vector4 a _ c d) = (Vector4 a b' c d)

instance HasZ Vector4 where
  vz (Vector4 _ _ c _) = c
  uvz c' (Vector4 a b _ d) = (Vector4 a b c' d)

instance HasT Vector4 where
  vt (Vector4 _ _ _ d) = d
  uvt d' (Vector4 a b c _) = (Vector4 a b c d')

instance HasX Vector5 where
  vx (Vector5 a _ _ _ _) = a
  uvx a' (Vector5 _ b c d e) = (Vector5 a' b c d e)

instance HasY Vector5 where
  vy (Vector5 _ b _ _ _) = b
  uvy b' (Vector5 a _ c d e) = (Vector5 a b' c d e)

instance HasZ Vector5 where
  vz (Vector5 _ _ c _ _) = c
  uvz c' (Vector5 a b _ d e) = (Vector5 a b c' d e)

instance HasT Vector5 where
  vt (Vector5 _ _ _ d _) = d
  uvt d' (Vector5 a b c _ e) = (Vector5 a b c d' e)

instance HasU Vector5 where
  vu (Vector5 _ _ _ _ e) = e
  uvu e' (Vector5 a b c d _) = (Vector5 a b c d e')

instance HasX Vector6 where
  vx (Vector6 a _ _ _ _ _) = a
  uvx a' (Vector6 _ b c d e f) = (Vector6 a' b c d e f)

instance HasY Vector6 where
  vy (Vector6 _ b _ _ _ _) = b
  uvy b' (Vector6 a _ c d e f) = (Vector6 a b' c d e f)

instance HasZ Vector6 where
  vz (Vector6 _ _ c _ _ _) = c
  uvz c' (Vector6 a b _ d e f) = (Vector6 a b c' d e f)

instance HasT Vector6 where
  vt (Vector6 _ _ _ d _ _) = d
  uvt d' (Vector6 a b c _ e f) = (Vector6 a b c d' e f)

instance HasU Vector6 where
  vu (Vector6 _ _ _ _ e _) = e
  uvu e' (Vector6 a b c d _ f) = (Vector6 a b c d e' f)

instance HasV Vector6 where
  vv (Vector6 _ _ _ _ _ f) = f
  uvv f' (Vector6 a b c d e _) = (Vector6 a b c d e f')

instance HasX Vector7 where
  vx (Vector7 a _ _ _ _ _ _) = a
  uvx a' (Vector7 _ b c d e f g) = (Vector7 a' b c d e f g)

instance HasY Vector7 where
  vy (Vector7 _ b _ _ _ _ _) = b
  uvy b' (Vector7 a _ c d e f g) = (Vector7 a b' c d e f g)

instance HasZ Vector7 where
  vz (Vector7 _ _ c _ _ _ _) = c
  uvz c' (Vector7 a b _ d e f g) = (Vector7 a b c' d e f g)

instance HasT Vector7 where
  vt (Vector7 _ _ _ d _ _ _) = d
  uvt d' (Vector7 a b c _ e f g) = (Vector7 a b c d' e f g)

instance HasU Vector7 where
  vu (Vector7 _ _ _ _ e _ _) = e
  uvu e' (Vector7 a b c d _ f g) = (Vector7 a b c d e' f g)

instance HasV Vector7 where
  vv (Vector7 _ _ _ _ _ f _) = f
  uvv f' (Vector7 a b c d e _ g) = (Vector7 a b c d e f' g)

instance HasW Vector7 where
  vw (Vector7 _ _ _ _ _ _ g) = g
  uvw g' (Vector7 a b c d e f _) = (Vector7 a b c d e f g')

instance HasX Vector8 where
  vx (Vector8 a _ _ _ _ _ _ _) = a
  uvx a' (Vector8 _ b c d e f g h) = (Vector8 a' b c d e f g h)

instance HasY Vector8 where
  vy (Vector8 _ b _ _ _ _ _ _) = b
  uvy b' (Vector8 a _ c d e f g h) = (Vector8 a b' c d e f g h)

instance HasZ Vector8 where
  vz (Vector8 _ _ c _ _ _ _ _) = c
  uvz c' (Vector8 a b _ d e f g h) = (Vector8 a b c' d e f g h)

instance HasT Vector8 where
  vt (Vector8 _ _ _ d _ _ _ _) = d
  uvt d' (Vector8 a b c _ e f g h) = (Vector8 a b c d' e f g h)

instance HasU Vector8 where
  vu (Vector8 _ _ _ _ e _ _ _) = e
  uvu e' (Vector8 a b c d _ f g h) = (Vector8 a b c d e' f g h)

instance HasV Vector8 where
  vv (Vector8 _ _ _ _ _ f _ _) = f
  uvv f' (Vector8 a b c d e _ g h) = (Vector8 a b c d e f' g h)

instance HasW Vector8 where
  vw (Vector8 _ _ _ _ _ _ g _) = g
  uvw g' (Vector8 a b c d e f _ h) = (Vector8 a b c d e f g' h)

instance HasX Vector9 where
  vx (Vector9 a _ _ _ _ _ _ _ _) = a
  uvx a' (Vector9 _ b c d e f g h i) = (Vector9 a' b c d e f g h i)

instance HasY Vector9 where
  vy (Vector9 _ b _ _ _ _ _ _ _) = b
  uvy b' (Vector9 a _ c d e f g h i) = (Vector9 a b' c d e f g h i)

instance HasZ Vector9 where
  vz (Vector9 _ _ c _ _ _ _ _ _) = c
  uvz c' (Vector9 a b _ d e f g h i) = (Vector9 a b c' d e f g h i)

instance HasT Vector9 where
  vt (Vector9 _ _ _ d _ _ _ _ _) = d
  uvt d' (Vector9 a b c _ e f g h i) = (Vector9 a b c d' e f g h i)

instance HasU Vector9 where
  vu (Vector9 _ _ _ _ e _ _ _ _) = e
  uvu e' (Vector9 a b c d _ f g h i) = (Vector9 a b c d e' f g h i)

instance HasV Vector9 where
  vv (Vector9 _ _ _ _ _ f _ _ _) = f
  uvv f' (Vector9 a b c d e _ g h i) = (Vector9 a b c d e f' g h i)

instance HasW Vector9 where
  vw (Vector9 _ _ _ _ _ _ g _ _) = g
  uvw g' (Vector9 a b c d e f _ h i) = (Vector9 a b c d e f g' h i)

instance HasX Vector10 where
  vx (Vector10 a _ _ _ _ _ _ _ _ _) = a
  uvx a' (Vector10 _ b c d e f g h i j) = (Vector10 a' b c d e f g h i j)

instance HasY Vector10 where
  vy (Vector10 _ b _ _ _ _ _ _ _ _) = b
  uvy b' (Vector10 a _ c d e f g h i j) = (Vector10 a b' c d e f g h i j)

instance HasZ Vector10 where
  vz (Vector10 _ _ c _ _ _ _ _ _ _) = c
  uvz c' (Vector10 a b _ d e f g h i j) = (Vector10 a b c' d e f g h i j)

instance HasT Vector10 where
  vt (Vector10 _ _ _ d _ _ _ _ _ _) = d
  uvt d' (Vector10 a b c _ e f g h i j) = (Vector10 a b c d' e f g h i j)

instance HasU Vector10 where
  vu (Vector10 _ _ _ _ e _ _ _ _ _) = e
  uvu e' (Vector10 a b c d _ f g h i j) = (Vector10 a b c d e' f g h i j)

instance HasV Vector10 where
  vv (Vector10 _ _ _ _ _ f _ _ _ _) = f
  uvv f' (Vector10 a b c d e _ g h i j) = (Vector10 a b c d e f' g h i j)

instance HasW Vector10 where
  vw (Vector10 _ _ _ _ _ _ g _ _ _) = g
  uvw g' (Vector10 a b c d e f _ h i j) = (Vector10 a b c d e f g' h i j)

instance HasX Vector11 where
  vx (Vector11 a _ _ _ _ _ _ _ _ _ _) = a
  uvx a' (Vector11 _ b c d e f g h i j k) = (Vector11 a' b c d e f g h i j k)

instance HasY Vector11 where
  vy (Vector11 _ b _ _ _ _ _ _ _ _ _) = b
  uvy b' (Vector11 a _ c d e f g h i j k) = (Vector11 a b' c d e f g h i j k)

instance HasZ Vector11 where
  vz (Vector11 _ _ c _ _ _ _ _ _ _ _) = c
  uvz c' (Vector11 a b _ d e f g h i j k) = (Vector11 a b c' d e f g h i j k)

instance HasT Vector11 where
  vt (Vector11 _ _ _ d _ _ _ _ _ _ _) = d
  uvt d' (Vector11 a b c _ e f g h i j k) = (Vector11 a b c d' e f g h i j k)

instance HasU Vector11 where
  vu (Vector11 _ _ _ _ e _ _ _ _ _ _) = e
  uvu e' (Vector11 a b c d _ f g h i j k) = (Vector11 a b c d e' f g h i j k)

instance HasV Vector11 where
  vv (Vector11 _ _ _ _ _ f _ _ _ _ _) = f
  uvv f' (Vector11 a b c d e _ g h i j k) = (Vector11 a b c d e f' g h i j k)

instance HasW Vector11 where
  vw (Vector11 _ _ _ _ _ _ g _ _ _ _) = g
  uvw g' (Vector11 a b c d e f _ h i j k) = (Vector11 a b c d e f g' h i j k)

instance HasX Vector12 where
  vx (Vector12 a _ _ _ _ _ _ _ _ _ _ _) = a
  uvx a' (Vector12 _ b c d e f g h i j k l) = (Vector12 a' b c d e f g h i j k l)

instance HasY Vector12 where
  vy (Vector12 _ b _ _ _ _ _ _ _ _ _ _) = b
  uvy b' (Vector12 a _ c d e f g h i j k l) = (Vector12 a b' c d e f g h i j k l)

instance HasZ Vector12 where
  vz (Vector12 _ _ c _ _ _ _ _ _ _ _ _) = c
  uvz c' (Vector12 a b _ d e f g h i j k l) = (Vector12 a b c' d e f g h i j k l)

instance HasT Vector12 where
  vt (Vector12 _ _ _ d _ _ _ _ _ _ _ _) = d
  uvt d' (Vector12 a b c _ e f g h i j k l) = (Vector12 a b c d' e f g h i j k l)

instance HasU Vector12 where
  vu (Vector12 _ _ _ _ e _ _ _ _ _ _ _) = e
  uvu e' (Vector12 a b c d _ f g h i j k l) = (Vector12 a b c d e' f g h i j k l)

instance HasV Vector12 where
  vv (Vector12 _ _ _ _ _ f _ _ _ _ _ _) = f
  uvv f' (Vector12 a b c d e _ g h i j k l) = (Vector12 a b c d e f' g h i j k l)

instance HasW Vector12 where
  vw (Vector12 _ _ _ _ _ _ g _ _ _ _ _) = g
  uvw g' (Vector12 a b c d e f _ h i j k l) = (Vector12 a b c d e f g' h i j k l)

instance HasX Vector13 where
  vx (Vector13 a _ _ _ _ _ _ _ _ _ _ _ _) = a
  uvx a' (Vector13 _ b c d e f g h i j k l m) = (Vector13 a' b c d e f g h i j k l m)

instance HasY Vector13 where
  vy (Vector13 _ b _ _ _ _ _ _ _ _ _ _ _) = b
  uvy b' (Vector13 a _ c d e f g h i j k l m) = (Vector13 a b' c d e f g h i j k l m)

instance HasZ Vector13 where
  vz (Vector13 _ _ c _ _ _ _ _ _ _ _ _ _) = c
  uvz c' (Vector13 a b _ d e f g h i j k l m) = (Vector13 a b c' d e f g h i j k l m)

instance HasT Vector13 where
  vt (Vector13 _ _ _ d _ _ _ _ _ _ _ _ _) = d
  uvt d' (Vector13 a b c _ e f g h i j k l m) = (Vector13 a b c d' e f g h i j k l m)

instance HasU Vector13 where
  vu (Vector13 _ _ _ _ e _ _ _ _ _ _ _ _) = e
  uvu e' (Vector13 a b c d _ f g h i j k l m) = (Vector13 a b c d e' f g h i j k l m)

instance HasV Vector13 where
  vv (Vector13 _ _ _ _ _ f _ _ _ _ _ _ _) = f
  uvv f' (Vector13 a b c d e _ g h i j k l m) = (Vector13 a b c d e f' g h i j k l m)

instance HasW Vector13 where
  vw (Vector13 _ _ _ _ _ _ g _ _ _ _ _ _) = g
  uvw g' (Vector13 a b c d e f _ h i j k l m) = (Vector13 a b c d e f g' h i j k l m)

instance HasX Vector14 where
  vx (Vector14 a _ _ _ _ _ _ _ _ _ _ _ _ _) = a
  uvx a' (Vector14 _ b c d e f g h i j k l m n) = (Vector14 a' b c d e f g h i j k l m n)

instance HasY Vector14 where
  vy (Vector14 _ b _ _ _ _ _ _ _ _ _ _ _ _) = b
  uvy b' (Vector14 a _ c d e f g h i j k l m n) = (Vector14 a b' c d e f g h i j k l m n)

instance HasZ Vector14 where
  vz (Vector14 _ _ c _ _ _ _ _ _ _ _ _ _ _) = c
  uvz c' (Vector14 a b _ d e f g h i j k l m n) = (Vector14 a b c' d e f g h i j k l m n)

instance HasT Vector14 where
  vt (Vector14 _ _ _ d _ _ _ _ _ _ _ _ _ _) = d
  uvt d' (Vector14 a b c _ e f g h i j k l m n) = (Vector14 a b c d' e f g h i j k l m n)

instance HasU Vector14 where
  vu (Vector14 _ _ _ _ e _ _ _ _ _ _ _ _ _) = e
  uvu e' (Vector14 a b c d _ f g h i j k l m n) = (Vector14 a b c d e' f g h i j k l m n)

instance HasV Vector14 where
  vv (Vector14 _ _ _ _ _ f _ _ _ _ _ _ _ _) = f
  uvv f' (Vector14 a b c d e _ g h i j k l m n) = (Vector14 a b c d e f' g h i j k l m n)

instance HasW Vector14 where
  vw (Vector14 _ _ _ _ _ _ g _ _ _ _ _ _ _) = g
  uvw g' (Vector14 a b c d e f _ h i j k l m n) = (Vector14 a b c d e f g' h i j k l m n)

instance HasX Vector15 where
  vx (Vector15 a _ _ _ _ _ _ _ _ _ _ _ _ _ _) = a
  uvx a' (Vector15 _ b c d e f g h i j k l m n o) = (Vector15 a' b c d e f g h i j k l m n o)

instance HasY Vector15 where
  vy (Vector15 _ b _ _ _ _ _ _ _ _ _ _ _ _ _) = b
  uvy b' (Vector15 a _ c d e f g h i j k l m n o) = (Vector15 a b' c d e f g h i j k l m n o)

instance HasZ Vector15 where
  vz (Vector15 _ _ c _ _ _ _ _ _ _ _ _ _ _ _) = c
  uvz c' (Vector15 a b _ d e f g h i j k l m n o) = (Vector15 a b c' d e f g h i j k l m n o)

instance HasT Vector15 where
  vt (Vector15 _ _ _ d _ _ _ _ _ _ _ _ _ _ _) = d
  uvt d' (Vector15 a b c _ e f g h i j k l m n o) = (Vector15 a b c d' e f g h i j k l m n o)

instance HasU Vector15 where
  vu (Vector15 _ _ _ _ e _ _ _ _ _ _ _ _ _ _) = e
  uvu e' (Vector15 a b c d _ f g h i j k l m n o) = (Vector15 a b c d e' f g h i j k l m n o)

instance HasV Vector15 where
  vv (Vector15 _ _ _ _ _ f _ _ _ _ _ _ _ _ _) = f
  uvv f' (Vector15 a b c d e _ g h i j k l m n o) = (Vector15 a b c d e f' g h i j k l m n o)

instance HasW Vector15 where
  vw (Vector15 _ _ _ _ _ _ g _ _ _ _ _ _ _ _) = g
  uvw g' (Vector15 a b c d e f _ h i j k l m n o) = (Vector15 a b c d e f g' h i j k l m n o)

instance HasX Vector16 where
  vx (Vector16 a _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = a
  uvx a' (Vector16 _ b c d e f g h i j k l m n o p) = (Vector16 a' b c d e f g h i j k l m n o p)

instance HasY Vector16 where
  vy (Vector16 _ b _ _ _ _ _ _ _ _ _ _ _ _ _ _) = b
  uvy b' (Vector16 a _ c d e f g h i j k l m n o p) = (Vector16 a b' c d e f g h i j k l m n o p)

instance HasZ Vector16 where
  vz (Vector16 _ _ c _ _ _ _ _ _ _ _ _ _ _ _ _) = c
  uvz c' (Vector16 a b _ d e f g h i j k l m n o p) = (Vector16 a b c' d e f g h i j k l m n o p)

instance HasT Vector16 where
  vt (Vector16 _ _ _ d _ _ _ _ _ _ _ _ _ _ _ _) = d
  uvt d' (Vector16 a b c _ e f g h i j k l m n o p) = (Vector16 a b c d' e f g h i j k l m n o p)

instance HasU Vector16 where
  vu (Vector16 _ _ _ _ e _ _ _ _ _ _ _ _ _ _ _) = e
  uvu e' (Vector16 a b c d _ f g h i j k l m n o p) = (Vector16 a b c d e' f g h i j k l m n o p)

instance HasV Vector16 where
  vv (Vector16 _ _ _ _ _ f _ _ _ _ _ _ _ _ _ _) = f
  uvv f' (Vector16 a b c d e _ g h i j k l m n o p) = (Vector16 a b c d e f' g h i j k l m n o p)

instance HasW Vector16 where
  vw (Vector16 _ _ _ _ _ _ g _ _ _ _ _ _ _ _ _) = g
  uvw g' (Vector16 a b c d e f _ h i j k l m n o p) = (Vector16 a b c d e f g' h i j k l m n o p)

  
