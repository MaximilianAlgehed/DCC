{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import DCC
import Lattice
import Control.Monad hiding (join)

class Relabelable a b where
  relabel :: a -> b

instance Relabelable () () where
  relabel () = ()

instance (l <= l') ~ True => Relabelable (T l a) (T l' a) where
  relabel t = t >>>= return

instance (Relabelable s s', Relabelable t t') => Relabelable (s, t) (s', t') where
  relabel (s, t) = (relabel s, relabel t)

instance Relabelable s s' => Relabelable (t -> s) (t -> s') where
  relabel f = \x -> relabel (f x)

fmap' :: l <= h ~ True => (a -> b) -> T l a -> T h b
fmap' f t = t >>>= (return . f)

join :: (s `ProtectedAt` l ~ True, Relabelable s s) => T l s -> s
join m = m >>>= relabel 
