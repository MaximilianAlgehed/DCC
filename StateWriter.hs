{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
import Lattice
import DCC
import Control.Monad.Writer
import Control.Monad.State

type LL = L
type HH = H

instance Monoid m => Monoid (T l m) where
  mempty   = return mempty
  mappend  = liftM2 mappend

type DBEntry = String
type Outputs e =  (T LL e, T HH e)

type Entries  = Outputs [DBEntry]

class Project (l :: Lattice) where
  type OutputsAbove l e
  project  :: Outputs e -> (T l e, OutputsAbove l e)
  merge    :: (T l e, OutputsAbove l e) -> Outputs e -> Outputs e

instance Project H where
  type OutputsAbove H o = ()
  project (_, h) = (h, ())
  merge (h, _) (l, _)   = (l, h)

instance Project L where
  type OutputsAbove L o = T H o
  project (l, h)   = (l, h)
  merge (l, h) _ = (l, h)

fill' p = merge (p, mempty) mempty

fill p = merge p mempty

teller
  :: (Project l,
      Monoid o,
      Monoid (OutputsAbove l o))
  => T l o -> WT o h ()
teller t = tell (fill' t)

type WT o l a = WriterT (Outputs o) (T l) a

type StT s l a = StateT (Outputs s) (T l) a

runStT :: StT s l a -> Outputs s -> T l (a, Outputs s)
runStT s m = runStateT s m

runWT :: WT o l a -> T l (a, Outputs o)
runWT m = runWriterT m

cast :: forall l h a o.
      ( Project h
      , ((OutputsAbove h o) `ProtectedAt` h) ~ True
      , Monoid o
      ) => WT o h a -> WT o l (T h a)
cast m = do
  let
    (newLog :: (T h o, OutputsAbove h o), lab) = runWT m >>>=
      \(a, newLog) -> (project newLog, return a)
  tell (fill newLog)
  return lab

castStT :: forall l h a s.
         ( Project h
         , ((OutputsAbove h s) `ProtectedAt` h) ~ True
         ) => StT s h a -> StT s l (T h a)
castStT m = do
  s <- get
  let (newSt :: (T h s, OutputsAbove h s), lab) = runStT m s >>>= \(a, st) -> (project st, return a)
  put (merge newSt s)
  return lab
