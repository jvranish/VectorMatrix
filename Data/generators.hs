
import Data.List
import Data.Char

sZip a b = zipWith (++) a b 

--sZip (cycle "Vector") (take n $ map show [1..]) 

--buildVectorTypes n = concatMap (buildVectorDataType "Vector") [1..n]

buildVectorDataType n = "data " ++ typeName ++ " a = " ++ apply typeName (replicate n "!a") ++ " deriving (Show, Read, Eq, Ord)\n"
  where
    typeName = "Vector" ++ show n
    

--"  fmap f (" ++ typeName ++ concat (intersperse " " (take n names)) ++ ") = " ++ typeName ++ concat [" (f " ++ x ++ ")" | x < names]
--instanceHeader className typeName = "instance Functor " ++ typeName ++ " where\n"

--data Vector2 a = Vector2 a a deriving (Show, Eq, Ord)
buildNumInstance n = "instance (Num a) => Num (Vector" ++ show n ++ " a) where\n\
  \  a + b = pure (+) <*> a <*> b\n\
  \  a - b = pure (-) <*> a <*> b\n\
  \  a * b = pure (*) <*> a <*> b\n\
  \  negate a = pure negate <*> a\n\
  \  abs a = pure abs <*> a\n\
  \  signum = fmap signum\n\
  \  fromInteger = pure . fromInteger\n\n"

buildFractionalInstance n = "instance (Fractional a) => Fractional (Vector" ++ show n ++ " a) where\n\
\  a / b = pure (/) <*> a <*> b\n\
\  recip a = pure 1 / a\n\
\  fromRational = pure . fromRational\n\n"

names = [x:y | y <- "" : map show [1..], x <- "abcdefghijklmnopqrstuvwxyz" ]

--instance Functor Vector1 where
--  fmap f (Vector1 x) = Vector1 (f x)

instanceHeader className typeName = "instance " ++ className ++ " " ++ typeName ++ " where\n"
applyMap fName params = fmap ((fName ++ " ") ++) params
applyZip fNames params = zipWith (\x y -> x ++ " " ++ y) fNames params
apply :: String -> [String] -> String
apply fName params = fName ++ " " ++ concat (intersperse " " params)
parens stuff = "(" ++ stuff ++ ")"

buildVectorFunctorInstance n = "instance Functor " ++ typeName ++ " where\n\
\  fmap f' (" ++ apply typeName currentNames ++ ") = " ++ apply typeName (fmap parens $ applyMap "f'" currentNames) ++ "\n"
  where
    currentNames = take n names
    typeName = "Vector" ++ show n

buildVectorApplicativeInstance n = "instance Applicative Vector" ++ show n ++ " where\n\
\  pure = return\n\
\  (<*>) = ap\n"

-- buildVectorApplicativeInstance n = "instance Applicative " ++ typeName ++ " where\n\
-- \  pure a = " ++ typeName ++ concat (replicate n " a") ++ "\n\
-- \  " ++  apply typeName functionNames ++ " <*> " ++ apply typeName parameterNames ++ " = " ++
   -- apply typeName (fmap parens $ applyZip functionNames parameterNames) ++"\n"
  -- where
    -- (parameterNames, functionNames) = splitAt n (take (n * 2) names)
    -- typeName = "Vector" ++ show n
    
buildVectorMonadInstance n = "instance Monad " ++ typeName ++ " where\n\
\  return a = " ++ typeName ++ concat (replicate n " a") ++ "\n\
\  " ++  apply typeName parameterNames ++ " >>= f' = case " ++ apply typeName (fmap parens $ applyMap "f'" parameterNames) ++ " of\n\
\    " ++  apply typeName thingy ++ " -> " ++ apply typeName diagNames ++ "\n"
  where
    thingy = fmap (parens . apply typeName) blanks
    blanks = [replaceAt dimN (diagNames !! dimN) (replicate n "_") | dimN <- [0..n-1]]
    (parameterNames, diagNames) = splitAt n (take (n * 2) names)
    typeName = "Vector" ++ show n

    --apply typeName parameterNames

buildVectorFoldableInstance n = "instance Foldable " ++  typeName ++ " where\n" ++
  "  foldMap f' (" ++ apply typeName currentNames ++ ") = " ++ concat (intersperse " `mappend` " (applyMap "f'" currentNames)) ++ "\n"
  where
    currentNames = take n names
    typeName = "Vector" ++ show n
    
buildVectorTraversableInstance n = instanceHeader "Traversable" typeName ++ 
  "  traverse f' (" ++ apply typeName currentNames ++ ") = " ++ typeName ++ " <$> " ++ 
  concat (intersperse " <*> " (applyMap "f'" currentNames)) ++ "\n"
  where
    currentNames = take n names
    typeName = "Vector" ++ show n
--  instance Traversable Vector2 where
--  traverse f (Vector2 a b) = Vector2 <$> f a <*> f b

--buildInstance "Functor" "Vector3" [
--    "fmap" "f" (apply Vector3 names) " = " Vector3 
--  ]

buildSizeClass n = 
  "class Size" ++ show n ++ " f where\n" ++
  "  packV" ++ show n ++ " :: " ++ (concat $ intersperse " -> " (replicate n "a")) ++ " -> f a\n" ++
  "  unpackV" ++ show n ++ " :: f a -> (" ++ (concat $ intersperse ", " (replicate n "a")) ++ ")\n\n"
  
buildVectorSizeInstance n = 
  instanceHeader ("Size" ++ show n) typeName ++
  "  packV" ++ show n ++ " " ++ concat (intersperse " " currentNames) ++ " = " ++ packed ++ "\n" ++
  "  unpackV" ++ show n ++ " (" ++ packed ++ ") = (" ++ (concat $ intersperse ", " currentNames) ++ ")\n\n"
  where
    typeName = "Vector" ++ show n
    currentNames = take n names
    packed = apply typeName currentNames

buildFixedListSizeInstance n = 
  instanceHeader ("Size" ++ show n) (unroll n) ++
  "  packV" ++ show n ++ " " ++ concat (intersperse " " currentNames) ++ " = " ++ packed ++ "\n" ++
  "  unpackV" ++ show n ++ " (" ++ packed ++ ") = (" ++ (concat $ intersperse ", " currentNames) ++ ")\n\n"
  where
    unroll 0 = "Nil"
    unroll n = parens $ "Cons " ++ unroll (n - 1)
    packed =  concat (intersperse " :. " (currentNames ++ ["Nil"]))
    currentNames = take n names
--buildFixedListSizeInstance 

buildDimensionClass :: Char -> String
buildDimensionClass dim = 
  "class Has" ++ [toUpper dim] ++ " f where\n" ++
  "  v" ++ [dim] ++ " :: f a -> a\n" ++
  "  uv" ++ [dim] ++ " :: a -> f a -> f a\n\n"

replaceAt n x xs = a ++ [x] ++ b
  where
    (a, _:b) = splitAt n xs

dims = "xyztuvw"
buildFixedListDimensionInstance n dimN = 
  instanceHeader ("Has" ++ [toUpper dim]) (unroll n) ++
  "  v" ++ [dim] ++ " (" ++ concat (intersperse " :. " (blanks ++ ["Nil"])) ++ ") = " ++ currentNames !! dimN ++ "\n" ++
  "  uv" ++ [dim] ++ " " ++ replacedVar ++ " (" ++ concat (intersperse " :. " (replaceAt dimN "_" currentNames ++ ["Nil"])) ++ ") = (" ++ 
       concat (intersperse " :. " ((replaceAt dimN replacedVar currentNames) ++ ["Nil"])) ++ ")\n\n"
  where
    unroll 0 = "Nil"
    unroll n = parens $ "Cons " ++ unroll (n - 1) 
    currentNames = take n names
    blanks = replaceAt dimN (currentNames !! dimN) (replicate n "_")
    dim = dims !! dimN
    replacedVar = currentNames !! dimN ++ "'" 
    
buildVectorDimensionInstance n dimN = 
  instanceHeader ("Has" ++ [toUpper dim]) typeName ++
  "  v" ++ [dim] ++ " (" ++ apply typeName blanks ++ ") = " ++ currentNames !! dimN ++ "\n" ++
  "  uv" ++ [dim] ++ " " ++ replacedVar ++ " (" ++ apply typeName (replaceAt dimN "_" currentNames) ++ ") = (" ++ 
      apply typeName (replaceAt dimN replacedVar currentNames) ++ ")\n\n"
  where
    currentNames = take n names
    blanks = replaceAt dimN (currentNames !! dimN) (replicate n "_")
    dim = dims !! dimN
    typeName = "Vector" ++ show n
    replacedVar = currentNames !! dimN ++ "'"
    
buildVectorDimensionInstances dims n = concatMap (buildVectorDimensionInstance n) (take n dims)
buildFixedListDimensionInstances dims n = concatMap (buildFixedListDimensionInstance n) (take n dims)

largestSize = 16
sizes = [1..largestSize]
dimensions = "xyztuvw"
dimNs = zipWith const [0..] dimensions
vectorDataTypes = concatMap buildVectorDataType sizes
vectorFunctorInstances = concatMap buildVectorFunctorInstance sizes
vectorApplicativeInstances = concatMap buildVectorApplicativeInstance sizes
vectorMonadInstances = concatMap buildVectorMonadInstance sizes
vectorFoldableInstances = concatMap buildVectorFoldableInstance sizes
vectorTraversableInstances = concatMap buildVectorTraversableInstance sizes
vectorNumInstances = concatMap buildNumInstance sizes
vectorFractionalInstances = concatMap buildFractionalInstance sizes

sizeClasses = concatMap buildSizeClass sizes
vectorSizeInstances = concatMap buildVectorSizeInstance sizes
fixedListSizeInstances = concatMap buildFixedListSizeInstance sizes

dimensionClasses = concatMap buildDimensionClass dimensions
vectorDimensionInstances = concatMap (buildVectorDimensionInstances dimNs) sizes
fixedListDimensionInstances = concatMap (buildFixedListDimensionInstances dimNs) sizes

header = "module Data.Instances where\n\n\
\import Control.Applicative\n\
\import Data.Monoid\n\
\import Data.Foldable\n\
\import Data.Traversable\n\
\import Data.FixedList\n\n"

buildStuff = header ++ vectorDataTypes ++ vectorFunctorInstances ++ vectorMonadInstances ++ vectorApplicativeInstances ++ vectorFoldableInstances ++
  vectorTraversableInstances ++ vectorNumInstances ++ vectorFractionalInstances ++ sizeClasses ++ 
  vectorSizeInstances ++ fixedListSizeInstances ++ dimensionClasses ++ vectorDimensionInstances ++ fixedListDimensionInstances
  
main = putStr $ buildStuff 
{-
packV3 :: a -> a -> a-> f a
unpackV3 :: f a -> (a, a, a)

instance Length4 Vector4 where

instance Length4 (Cons (Cons (Cons (Cons Nil)))) where

class Length3 f where
  build3 :: a -> a -> a -> f a

class Length4 f where
  build4 :: a -> a -> a -> a -> f a

class HasX f where
  vx :: f a -> a
  uvx :: a -> f a -> f a

instance HasX Vector4 where
  vx (Vector4 a b c d) = a
  uvx x' (Vector4 a b c d) = Vector4 x b c d

class HasY f where
  vx :: f a -> a
-}
{-
fmap (parens . sZip "f") (take n names)

fmap (apply "f" 

where
  asdf x = "(f " ++ x ++ ")"
instance Functor Vector3 where
  fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)
instance Functor Vector4 where
  fmap f (Vector4 x y z w) = Vector4 (f x) (f y) (f z) (f w)

instance Applicative Vector1 where
  pure x = Vector1 x
  Vector1 f <*> Vector1 x = Vector1 (f x)
instance Applicative Vector2 where
  pure x = Vector2 x x
  Vector2 f g <*> Vector2 x y = Vector2 (f x) (g y)
instance Applicative Vector3 where
  pure x = Vector3 x x x
  Vector3 f g h <*> Vector3 x y z = Vector3 (f x) (g y) (h z)
instance Applicative Vector4 where
  pure x = Vector4 x x x x
  Vector4 f g h i <*> Vector4 x y z w = Vector4 (f x) (g y) (h z) (i w)

instance Foldable Vector1 where
  foldMap f (Vector1 a) = f a  
instance Foldable Vector2 where
  foldMap f (Vector2 a b) = f a `mappend` f b
instance Foldable Vector3 where
  foldMap f (Vector3 a b c) = f a `mappend` f b `mappend` f c
instance Foldable Vector4 where
  foldMap f (Vector4 a b c d) = f a `mappend` f b `mappend` f c `mappend` f d
  
instance Traversable Vector1 where
  traverse f (Vector1 a) = Vector1 <$> f a
instance Traversable Vector2 where
  traverse f (Vector2 a b) = Vector2 <$> f a <*> f b
  
  -}
