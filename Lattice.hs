{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Lattice where

-- Two point lattice
data Lattice = H | L deriving (Show, Eq)

type family l <= h where
  H <= L = False
  _ <= _ = True

leq :: Lattice -> Lattice -> Bool
L `leq` _ = True
_ `leq` H = True
_ `leq` _ = False

lub :: Lattice -> Lattice -> Lattice
L `lub` x = x
x `lub` L = x
H `lub` _ = H
