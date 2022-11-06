{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.State.Church where

import Control.Carrier.State.Church
import Grisette.Core

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GMergeable bool (StateC e m a)
  where
  gmergingStrategy = SimpleStrategy $ \cond (StateC l) (StateC r) ->
    StateC $ \k s -> unionIf cond (l k s) (r k s)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GMergeable1 bool (StateC e m)
  where
  liftGMergingStrategy _ = SimpleStrategy $ \cond (StateC l) (StateC r) ->
    StateC $ \k s -> unionIf cond (l k s) (r k s)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GSimpleMergeable bool (StateC e m a)
  where
  gmrgIte bool (StateC l) (StateC r) = StateC $ \k s ->
    unionIf bool (l k s) (r k s)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GSimpleMergeable1 bool (StateC e m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GUnionLike bool (StateC e m)
  where
  mergeWithStrategy _ = id
  mrgIfWithStrategy _ = unionIf
  mrgSingleWithStrategy _ = single
  single a = StateC $ \k s -> k s a
  unionIf cond (StateC l) (StateC r) =
    StateC $ \k s -> unionIf cond (l k s) (r k s)

mrgRunState ::
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool b) =>
  (s -> a -> m b) ->
  s ->
  StateC s m a ->
  m b
mrgRunState f s = merge . runState f s

mrgEvalState ::
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool a) =>
  s ->
  StateC s m a ->
  m a
mrgEvalState = runState (const mrgSingle)

mrgExecState ::
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool s) =>
  s ->
  StateC s m a ->
  m s
mrgExecState = runState (const . mrgSingle)
