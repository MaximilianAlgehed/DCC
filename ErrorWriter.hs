{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
import Lattice
import DCC
import Control.Monad.Writer
import Control.Monad.Except

type Outputs e = (T L e, T H e)

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

fill p  = merge p mempty

teller
  :: (Project l,
      Monoid o,
      Monoid (OutputsAbove l o))
  => T l o -> WET o h ()
teller t = tell (fill' t)

type WET o l a = WriterT (Outputs o) (ExceptT String (T l)) a

runWET :: WET o l a -> T l (Either String (a, Outputs o))
runWET = runExceptT . runWriterT

cast :: forall l h a o.
      ( Project h
      , ((OutputsAbove h o) `ProtectedAt` h) ~ True
      , Monoid o
      ) => WET o h a -> WET o l (T h (Either String a))
cast h = do
  let (outs :: (T h o, OutputsAbove h o),
       tha :: T h (Either String a)) =
        runWET h >>>= \e -> case e of
          Left s          -> (project mempty, return (Left s))
          Right (a, outs) -> (project outs, return (Right a))
  tell (fill outs)
  return tha
