{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.Throw.Either where

import Control.Carrier.Throw.Either
import Grisette.Core
import Grisette.Lib.Control.Carrier.Error.Either ()

instance
  (SymBoolOp bool, GMergeable1 bool m, GMergeable bool e, GMergeable bool a) =>
  GMergeable bool (ThrowC e m a)
  where
  gmergingStrategy = gwrapStrategy gmergingStrategy ThrowC (\(ThrowC et) -> et)

instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool e, Functor m) => GMergeable1 bool (ThrowC e m) where
  liftGMergingStrategy ms = gwrapStrategy (liftGMergingStrategy ms) ThrowC (\(ThrowC et) -> et)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e, GMergeable bool a, Functor m) =>
  GSimpleMergeable bool (ThrowC e m a)
  where
  gmrgIte cond (ThrowC t) (ThrowC f) = ThrowC $ gmrgIte cond t f

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e, Functor m) =>
  GSimpleMergeable1 bool (ThrowC e m)
  where
  liftGMrgIte s = mrgIfWithStrategy (SimpleStrategy s)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e, Functor m) =>
  GUnionLike bool (ThrowC e m)
  where
  mergeWithStrategy s (ThrowC et) = ThrowC $ mergeWithStrategy s et
  mrgIfWithStrategy s cond (ThrowC l) (ThrowC r) = ThrowC $ mrgIfWithStrategy s cond l r
  mrgSingleWithStrategy ms a = ThrowC $ mrgSingleWithStrategy ms a
  single a = ThrowC $ single a
  unionIf cond (ThrowC t) (ThrowC f) = ThrowC $ unionIf cond t f

mrgRunThrow ::
  ( SymBoolOp bool,
    GUnionLike bool m,
    GMergeable bool exc,
    GMergeable bool a
  ) =>
  ThrowC exc m a ->
  m (Either exc a)
mrgRunThrow = merge . runThrow
