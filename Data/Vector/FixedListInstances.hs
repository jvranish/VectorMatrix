{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Vector.FixedListInstances where

import Data.FixedList
import Data.Vector.Classes

instance Size1 (Cons Nil) where
  packV1 a = a :. Nil
  unpackV1 (a :. Nil) = (a)

instance Size2 (Cons (Cons Nil)) where
  packV2 a b = a :. b :. Nil
  unpackV2 (a :. b :. Nil) = (a, b)

instance Size3 (Cons (Cons (Cons Nil))) where
  packV3 a b c = a :. b :. c :. Nil
  unpackV3 (a :. b :. c :. Nil) = (a, b, c)

instance Size4 (Cons (Cons (Cons (Cons Nil)))) where
  packV4 a b c d = a :. b :. c :. d :. Nil
  unpackV4 (a :. b :. c :. d :. Nil) = (a, b, c, d)

instance Size5 (Cons (Cons (Cons (Cons (Cons Nil))))) where
  packV5 a b c d e = a :. b :. c :. d :. e :. Nil
  unpackV5 (a :. b :. c :. d :. e :. Nil) = (a, b, c, d, e)

instance Size6 (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))) where
  packV6 a b c d e f = a :. b :. c :. d :. e :. f :. Nil
  unpackV6 (a :. b :. c :. d :. e :. f :. Nil) = (a, b, c, d, e, f)

instance Size7 (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))) where
  packV7 a b c d e f g = a :. b :. c :. d :. e :. f :. g :. Nil
  unpackV7 (a :. b :. c :. d :. e :. f :. g :. Nil) = (a, b, c, d, e, f, g)

instance Size8 (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))) where
  packV8 a b c d e f g h = a :. b :. c :. d :. e :. f :. g :. h :. Nil
  unpackV8 (a :. b :. c :. d :. e :. f :. g :. h :. Nil) = (a, b, c, d, e, f, g, h)

instance Size9 (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))) where
  packV9 a b c d e f g h i = a :. b :. c :. d :. e :. f :. g :. h :. i :. Nil
  unpackV9 (a :. b :. c :. d :. e :. f :. g :. h :. i :. Nil) = (a, b, c, d, e, f, g, h, i)

instance Size10 (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))) where
  packV10 a b c d e f g h i j = a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. Nil
  unpackV10 (a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. Nil) = (a, b, c, d, e, f, g, h, i, j)

instance Size11 (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))) where
  packV11 a b c d e f g h i j k = a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. Nil
  unpackV11 (a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. Nil) = (a, b, c, d, e, f, g, h, i, j, k)

instance Size12 (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))) where
  packV12 a b c d e f g h i j k l = a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. Nil
  unpackV12 (a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. Nil) = (a, b, c, d, e, f, g, h, i, j, k, l)

instance Size13 (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))) where
  packV13 a b c d e f g h i j k l m = a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. Nil
  unpackV13 (a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. Nil) = (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance Size14 (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))) where
  packV14 a b c d e f g h i j k l m n = a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil
  unpackV14 (a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance Size15 (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))))) where
  packV15 a b c d e f g h i j k l m n o = a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil
  unpackV15 (a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance Size16 (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))))) where
  packV16 a b c d e f g h i j k l m n o p = a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil
  unpackV16 (a :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

instance HasX (Cons Nil) where
  vx (a :. Nil) = a
  uvx a' (_ :. Nil) = (a' :. Nil)

instance HasX (Cons (Cons Nil)) where
  vx (a :. _ :. Nil) = a
  uvx a' (_ :. b :. Nil) = (a' :. b :. Nil)

instance HasY (Cons (Cons Nil)) where
  vy (_ :. b :. Nil) = b
  uvy b' (a :. _ :. Nil) = (a :. b' :. Nil)

instance HasX (Cons (Cons (Cons Nil))) where
  vx (a :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. Nil) = (a' :. b :. c :. Nil)

instance HasY (Cons (Cons (Cons Nil))) where
  vy (_ :. b :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. Nil) = (a :. b' :. c :. Nil)

instance HasZ (Cons (Cons (Cons Nil))) where
  vz (_ :. _ :. c :. Nil) = c
  uvz c' (a :. b :. _ :. Nil) = (a :. b :. c' :. Nil)

instance HasX (Cons (Cons (Cons (Cons Nil)))) where
  vx (a :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. Nil) = (a' :. b :. c :. d :. Nil)

instance HasY (Cons (Cons (Cons (Cons Nil)))) where
  vy (_ :. b :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. Nil) = (a :. b' :. c :. d :. Nil)

instance HasZ (Cons (Cons (Cons (Cons Nil)))) where
  vz (_ :. _ :. c :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. Nil) = (a :. b :. c' :. d :. Nil)

instance HasT (Cons (Cons (Cons (Cons Nil)))) where
  vt (_ :. _ :. _ :. d :. Nil) = d
  uvt d' (a :. b :. c :. _ :. Nil) = (a :. b :. c :. d' :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons Nil))))) where
  vx (a :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. Nil) = (a' :. b :. c :. d :. e :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons Nil))))) where
  vy (_ :. b :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. Nil) = (a :. b' :. c :. d :. e :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons Nil))))) where
  vz (_ :. _ :. c :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. Nil) = (a :. b :. c' :. d :. e :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons Nil))))) where
  vt (_ :. _ :. _ :. d :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. Nil) = (a :. b :. c :. d' :. e :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons Nil))))) where
  vu (_ :. _ :. _ :. _ :. e :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. Nil) = (a :. b :. c :. d :. e' :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))) where
  vx (a :. _ :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. f :. Nil) = (a' :. b :. c :. d :. e :. f :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))) where
  vy (_ :. b :. _ :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. f :. Nil) = (a :. b' :. c :. d :. e :. f :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))) where
  vz (_ :. _ :. c :. _ :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. f :. Nil) = (a :. b :. c' :. d :. e :. f :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))) where
  vt (_ :. _ :. _ :. d :. _ :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. f :. Nil) = (a :. b :. c :. d' :. e :. f :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))) where
  vu (_ :. _ :. _ :. _ :. e :. _ :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. f :. Nil) = (a :. b :. c :. d :. e' :. f :. Nil)

instance HasV (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))) where
  vv (_ :. _ :. _ :. _ :. _ :. f :. Nil) = f
  uvv f' (a :. b :. c :. d :. e :. _ :. Nil) = (a :. b :. c :. d :. e :. f' :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))) where
  vx (a :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. f :. g :. Nil) = (a' :. b :. c :. d :. e :. f :. g :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))) where
  vy (_ :. b :. _ :. _ :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. f :. g :. Nil) = (a :. b' :. c :. d :. e :. f :. g :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))) where
  vz (_ :. _ :. c :. _ :. _ :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. f :. g :. Nil) = (a :. b :. c' :. d :. e :. f :. g :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))) where
  vt (_ :. _ :. _ :. d :. _ :. _ :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. f :. g :. Nil) = (a :. b :. c :. d' :. e :. f :. g :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))) where
  vu (_ :. _ :. _ :. _ :. e :. _ :. _ :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. f :. g :. Nil) = (a :. b :. c :. d :. e' :. f :. g :. Nil)

instance HasV (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))) where
  vv (_ :. _ :. _ :. _ :. _ :. f :. _ :. Nil) = f
  uvv f' (a :. b :. c :. d :. e :. _ :. g :. Nil) = (a :. b :. c :. d :. e :. f' :. g :. Nil)

instance HasW (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))) where
  vw (_ :. _ :. _ :. _ :. _ :. _ :. g :. Nil) = g
  uvw g' (a :. b :. c :. d :. e :. f :. _ :. Nil) = (a :. b :. c :. d :. e :. f :. g' :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))) where
  vx (a :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. f :. g :. h :. Nil) = (a' :. b :. c :. d :. e :. f :. g :. h :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))) where
  vy (_ :. b :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. f :. g :. h :. Nil) = (a :. b' :. c :. d :. e :. f :. g :. h :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))) where
  vz (_ :. _ :. c :. _ :. _ :. _ :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. f :. g :. h :. Nil) = (a :. b :. c' :. d :. e :. f :. g :. h :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))) where
  vt (_ :. _ :. _ :. d :. _ :. _ :. _ :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. f :. g :. h :. Nil) = (a :. b :. c :. d' :. e :. f :. g :. h :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))) where
  vu (_ :. _ :. _ :. _ :. e :. _ :. _ :. _ :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. f :. g :. h :. Nil) = (a :. b :. c :. d :. e' :. f :. g :. h :. Nil)

instance HasV (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))) where
  vv (_ :. _ :. _ :. _ :. _ :. f :. _ :. _ :. Nil) = f
  uvv f' (a :. b :. c :. d :. e :. _ :. g :. h :. Nil) = (a :. b :. c :. d :. e :. f' :. g :. h :. Nil)

instance HasW (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))) where
  vw (_ :. _ :. _ :. _ :. _ :. _ :. g :. _ :. Nil) = g
  uvw g' (a :. b :. c :. d :. e :. f :. _ :. h :. Nil) = (a :. b :. c :. d :. e :. f :. g' :. h :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))) where
  vx (a :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. f :. g :. h :. i :. Nil) = (a' :. b :. c :. d :. e :. f :. g :. h :. i :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))) where
  vy (_ :. b :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. f :. g :. h :. i :. Nil) = (a :. b' :. c :. d :. e :. f :. g :. h :. i :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))) where
  vz (_ :. _ :. c :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. f :. g :. h :. i :. Nil) = (a :. b :. c' :. d :. e :. f :. g :. h :. i :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))) where
  vt (_ :. _ :. _ :. d :. _ :. _ :. _ :. _ :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. f :. g :. h :. i :. Nil) = (a :. b :. c :. d' :. e :. f :. g :. h :. i :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))) where
  vu (_ :. _ :. _ :. _ :. e :. _ :. _ :. _ :. _ :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. f :. g :. h :. i :. Nil) = (a :. b :. c :. d :. e' :. f :. g :. h :. i :. Nil)

instance HasV (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))) where
  vv (_ :. _ :. _ :. _ :. _ :. f :. _ :. _ :. _ :. Nil) = f
  uvv f' (a :. b :. c :. d :. e :. _ :. g :. h :. i :. Nil) = (a :. b :. c :. d :. e :. f' :. g :. h :. i :. Nil)

instance HasW (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))) where
  vw (_ :. _ :. _ :. _ :. _ :. _ :. g :. _ :. _ :. Nil) = g
  uvw g' (a :. b :. c :. d :. e :. f :. _ :. h :. i :. Nil) = (a :. b :. c :. d :. e :. f :. g' :. h :. i :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))) where
  vx (a :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. f :. g :. h :. i :. j :. Nil) = (a' :. b :. c :. d :. e :. f :. g :. h :. i :. j :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))) where
  vy (_ :. b :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. f :. g :. h :. i :. j :. Nil) = (a :. b' :. c :. d :. e :. f :. g :. h :. i :. j :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))) where
  vz (_ :. _ :. c :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. f :. g :. h :. i :. j :. Nil) = (a :. b :. c' :. d :. e :. f :. g :. h :. i :. j :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))) where
  vt (_ :. _ :. _ :. d :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. f :. g :. h :. i :. j :. Nil) = (a :. b :. c :. d' :. e :. f :. g :. h :. i :. j :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))) where
  vu (_ :. _ :. _ :. _ :. e :. _ :. _ :. _ :. _ :. _ :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. f :. g :. h :. i :. j :. Nil) = (a :. b :. c :. d :. e' :. f :. g :. h :. i :. j :. Nil)

instance HasV (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))) where
  vv (_ :. _ :. _ :. _ :. _ :. f :. _ :. _ :. _ :. _ :. Nil) = f
  uvv f' (a :. b :. c :. d :. e :. _ :. g :. h :. i :. j :. Nil) = (a :. b :. c :. d :. e :. f' :. g :. h :. i :. j :. Nil)

instance HasW (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))) where
  vw (_ :. _ :. _ :. _ :. _ :. _ :. g :. _ :. _ :. _ :. Nil) = g
  uvw g' (a :. b :. c :. d :. e :. f :. _ :. h :. i :. j :. Nil) = (a :. b :. c :. d :. e :. f :. g' :. h :. i :. j :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))) where
  vx (a :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. Nil) = (a' :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))) where
  vy (_ :. b :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. f :. g :. h :. i :. j :. k :. Nil) = (a :. b' :. c :. d :. e :. f :. g :. h :. i :. j :. k :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))) where
  vz (_ :. _ :. c :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. f :. g :. h :. i :. j :. k :. Nil) = (a :. b :. c' :. d :. e :. f :. g :. h :. i :. j :. k :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))) where
  vt (_ :. _ :. _ :. d :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. f :. g :. h :. i :. j :. k :. Nil) = (a :. b :. c :. d' :. e :. f :. g :. h :. i :. j :. k :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))) where
  vu (_ :. _ :. _ :. _ :. e :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. f :. g :. h :. i :. j :. k :. Nil) = (a :. b :. c :. d :. e' :. f :. g :. h :. i :. j :. k :. Nil)

instance HasV (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))) where
  vv (_ :. _ :. _ :. _ :. _ :. f :. _ :. _ :. _ :. _ :. _ :. Nil) = f
  uvv f' (a :. b :. c :. d :. e :. _ :. g :. h :. i :. j :. k :. Nil) = (a :. b :. c :. d :. e :. f' :. g :. h :. i :. j :. k :. Nil)

instance HasW (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))) where
  vw (_ :. _ :. _ :. _ :. _ :. _ :. g :. _ :. _ :. _ :. _ :. Nil) = g
  uvw g' (a :. b :. c :. d :. e :. f :. _ :. h :. i :. j :. k :. Nil) = (a :. b :. c :. d :. e :. f :. g' :. h :. i :. j :. k :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))) where
  vx (a :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. Nil) = (a' :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))) where
  vy (_ :. b :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. Nil) = (a :. b' :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))) where
  vz (_ :. _ :. c :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. f :. g :. h :. i :. j :. k :. l :. Nil) = (a :. b :. c' :. d :. e :. f :. g :. h :. i :. j :. k :. l :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))) where
  vt (_ :. _ :. _ :. d :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. f :. g :. h :. i :. j :. k :. l :. Nil) = (a :. b :. c :. d' :. e :. f :. g :. h :. i :. j :. k :. l :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))) where
  vu (_ :. _ :. _ :. _ :. e :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. f :. g :. h :. i :. j :. k :. l :. Nil) = (a :. b :. c :. d :. e' :. f :. g :. h :. i :. j :. k :. l :. Nil)

instance HasV (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))) where
  vv (_ :. _ :. _ :. _ :. _ :. f :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = f
  uvv f' (a :. b :. c :. d :. e :. _ :. g :. h :. i :. j :. k :. l :. Nil) = (a :. b :. c :. d :. e :. f' :. g :. h :. i :. j :. k :. l :. Nil)

instance HasW (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))) where
  vw (_ :. _ :. _ :. _ :. _ :. _ :. g :. _ :. _ :. _ :. _ :. _ :. Nil) = g
  uvw g' (a :. b :. c :. d :. e :. f :. _ :. h :. i :. j :. k :. l :. Nil) = (a :. b :. c :. d :. e :. f :. g' :. h :. i :. j :. k :. l :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))) where
  vx (a :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. Nil) = (a' :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))) where
  vy (_ :. b :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. Nil) = (a :. b' :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))) where
  vz (_ :. _ :. c :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. Nil) = (a :. b :. c' :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))) where
  vt (_ :. _ :. _ :. d :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. f :. g :. h :. i :. j :. k :. l :. m :. Nil) = (a :. b :. c :. d' :. e :. f :. g :. h :. i :. j :. k :. l :. m :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))) where
  vu (_ :. _ :. _ :. _ :. e :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. f :. g :. h :. i :. j :. k :. l :. m :. Nil) = (a :. b :. c :. d :. e' :. f :. g :. h :. i :. j :. k :. l :. m :. Nil)

instance HasV (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))) where
  vv (_ :. _ :. _ :. _ :. _ :. f :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = f
  uvv f' (a :. b :. c :. d :. e :. _ :. g :. h :. i :. j :. k :. l :. m :. Nil) = (a :. b :. c :. d :. e :. f' :. g :. h :. i :. j :. k :. l :. m :. Nil)

instance HasW (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))) where
  vw (_ :. _ :. _ :. _ :. _ :. _ :. g :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = g
  uvw g' (a :. b :. c :. d :. e :. f :. _ :. h :. i :. j :. k :. l :. m :. Nil) = (a :. b :. c :. d :. e :. f :. g' :. h :. i :. j :. k :. l :. m :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))) where
  vx (a :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil) = (a' :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))) where
  vy (_ :. b :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil) = (a :. b' :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))) where
  vz (_ :. _ :. c :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil) = (a :. b :. c' :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))) where
  vt (_ :. _ :. _ :. d :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil) = (a :. b :. c :. d' :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))) where
  vu (_ :. _ :. _ :. _ :. e :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil) = (a :. b :. c :. d :. e' :. f :. g :. h :. i :. j :. k :. l :. m :. n :. Nil)

instance HasV (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))) where
  vv (_ :. _ :. _ :. _ :. _ :. f :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = f
  uvv f' (a :. b :. c :. d :. e :. _ :. g :. h :. i :. j :. k :. l :. m :. n :. Nil) = (a :. b :. c :. d :. e :. f' :. g :. h :. i :. j :. k :. l :. m :. n :. Nil)

instance HasW (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))) where
  vw (_ :. _ :. _ :. _ :. _ :. _ :. g :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = g
  uvw g' (a :. b :. c :. d :. e :. f :. _ :. h :. i :. j :. k :. l :. m :. n :. Nil) = (a :. b :. c :. d :. e :. f :. g' :. h :. i :. j :. k :. l :. m :. n :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))))) where
  vx (a :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil) = (a' :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))))) where
  vy (_ :. b :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil) = (a :. b' :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))))) where
  vz (_ :. _ :. c :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil) = (a :. b :. c' :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))))) where
  vt (_ :. _ :. _ :. d :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil) = (a :. b :. c :. d' :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))))) where
  vu (_ :. _ :. _ :. _ :. e :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil) = (a :. b :. c :. d :. e' :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil)

instance HasV (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))))) where
  vv (_ :. _ :. _ :. _ :. _ :. f :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = f
  uvv f' (a :. b :. c :. d :. e :. _ :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil) = (a :. b :. c :. d :. e :. f' :. g :. h :. i :. j :. k :. l :. m :. n :. o :. Nil)

instance HasW (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil))))))))))))))) where
  vw (_ :. _ :. _ :. _ :. _ :. _ :. g :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = g
  uvw g' (a :. b :. c :. d :. e :. f :. _ :. h :. i :. j :. k :. l :. m :. n :. o :. Nil) = (a :. b :. c :. d :. e :. f :. g' :. h :. i :. j :. k :. l :. m :. n :. o :. Nil)

instance HasX (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))))) where
  vx (a :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = a
  uvx a' (_ :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil) = (a' :. b :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil)

instance HasY (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))))) where
  vy (_ :. b :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = b
  uvy b' (a :. _ :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil) = (a :. b' :. c :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil)

instance HasZ (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))))) where
  vz (_ :. _ :. c :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = c
  uvz c' (a :. b :. _ :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil) = (a :. b :. c' :. d :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil)

instance HasT (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))))) where
  vt (_ :. _ :. _ :. d :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = d
  uvt d' (a :. b :. c :. _ :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil) = (a :. b :. c :. d' :. e :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil)

instance HasU (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))))) where
  vu (_ :. _ :. _ :. _ :. e :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = e
  uvu e' (a :. b :. c :. d :. _ :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil) = (a :. b :. c :. d :. e' :. f :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil)

instance HasV (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))))) where
  vv (_ :. _ :. _ :. _ :. _ :. f :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = f
  uvv f' (a :. b :. c :. d :. e :. _ :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil) = (a :. b :. c :. d :. e :. f' :. g :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil)

instance HasW (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Nil)))))))))))))))) where
  vw (_ :. _ :. _ :. _ :. _ :. _ :. g :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. Nil) = g
  uvw g' (a :. b :. c :. d :. e :. f :. _ :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil) = (a :. b :. c :. d :. e :. f :. g' :. h :. i :. j :. k :. l :. m :. n :. o :. p :. Nil)

