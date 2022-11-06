{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.State.Lazy where

import Control.Carrier.State.Lazy
import Grisette.Core

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (StateC s m a)
  where
  gmergingStrategy = gwrapStrategy (liftGMergingStrategy gmergingStrategy1) StateC (\(StateC f) -> f)

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable1 bool m) =>
  GMergeable1 bool (StateC s m)
  where
  liftGMergingStrategy s = gwrapStrategy (liftGMergingStrategy (liftGMergingStrategy (liftGMergingStrategy s))) StateC (\(StateC f) -> f)

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GUnionLike bool m) =>
  GSimpleMergeable bool (StateC s m a)
  where
  gmrgIte = mrgIf

instance
  (SymBoolOp bool, GMergeable bool s, GUnionLike bool m) =>
  GSimpleMergeable1 bool (StateC s m)
  where
  liftGMrgIte = mrgIfWithStrategy . SimpleStrategy

instance
  (SymBoolOp bool, GMergeable bool s, GUnionLike bool m) =>
  GUnionLike bool (StateC s m)
  where
  mergeWithStrategy ms (StateC f) = StateC $ mergeWithStrategy (liftGMergingStrategy ms) . f
  mrgIfWithStrategy s cond (StateC l) (StateC r) = StateC $ \v -> mrgIfWithStrategy (liftGMergingStrategy s) cond (l v) (r v)
  mrgSingleWithStrategy ms a = StateC $ \s -> mrgSingleWithStrategy (liftGMergingStrategy ms) (s, a)
  single a = StateC $ \s -> single (s, a)
  unionIf cond (StateC l) (StateC r) = StateC $ \s -> unionIf cond (l s) (r s)

mrgRunState :: (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GUnionLike bool m) => s -> StateC s m a -> m (s, a)
mrgRunState s = merge . runState s

mrgEvalState :: (SymBoolOp bool, GMergeable bool a, GUnionLike bool m, Functor m) => s -> StateC s m a -> m a
mrgEvalState s = merge . fmap snd . runState s

mrgExecState :: (SymBoolOp bool, GMergeable bool s, GUnionLike bool m, Functor m) => s -> StateC s m a -> m s
mrgExecState s = merge . fmap fst . runState s
