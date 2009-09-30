module Data.Vector.Lenses where

import Control.Monad.State
import Data.Vector.Classes
import Data.Lenses

vxl :: (HasX f, MonadState (f a) m) => StateT a m b -> m b
vxl = fromGetSet vx uvx
vyl :: (HasY f, MonadState (f a) m) => StateT a m b -> m b
vyl = fromGetSet vy uvy
vzl :: (HasZ f, MonadState (f a) m) => StateT a m b -> m b
vzl = fromGetSet vz uvz
vtl :: (HasT f, MonadState (f a) m) => StateT a m b -> m b
vtl = fromGetSet vt uvt
vul :: (HasU f, MonadState (f a) m) => StateT a m b -> m b
vul = fromGetSet vu uvu
vvl :: (HasV f, MonadState (f a) m) => StateT a m b -> m b
vvl = fromGetSet vv uvv
vwl :: (HasW f, MonadState (f a) m) => StateT a m b -> m b
vwl = fromGetSet vw uvw


