{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DataKinds     #-}

module Lattice where

-- Two point lattice
data Lattice = H | L deriving (Show, Eq)

type family l <= h where
  H <= L = False
  _ <= _ = True
