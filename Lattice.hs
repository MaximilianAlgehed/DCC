{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Lattice where

-- Two point lattice
data Lattice = H | L | X deriving (Show, Eq)

type family l <= h where
  l <= l = True -- This seems redundant, but we need it to make GHC
                -- okay with the Monad instance for `T` in DCC
  L <= l = True
  l <= H = True

leq :: Lattice -> Lattice -> Bool
L `leq` _ = True
_ `leq` H = True
_ `leq` _ = False

lub :: Lattice -> Lattice -> Lattice
L `lub` x = x
x `lub` L = x
H `lub` _ = H
