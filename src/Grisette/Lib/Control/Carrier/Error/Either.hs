{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.Error.Either (mrgRunError) where

import Control.Carrier.Error.Either
import Grisette.Core

instance
  (SymBoolOp bool, GMergeable1 bool m, GMergeable bool e, GMergeable bool a) =>
  GMergeable bool (ErrorC e m a)
  where
  gmergingStrategy = gwrapStrategy gmergingStrategy ErrorC (\(ErrorC et) -> et)

instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool e, Functor m) => GMergeable1 bool (ErrorC e m) where
  liftGMergingStrategy ms = gwrapStrategy (liftGMergingStrategy ms) ErrorC (\(ErrorC et) -> et)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e, GMergeable bool a, Functor m) =>
  GSimpleMergeable bool (ErrorC e m a)
  where
  gmrgIte cond (ErrorC t) (ErrorC f) = ErrorC $ gmrgIte cond t f

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e, Functor m) =>
  GSimpleMergeable1 bool (ErrorC e m)
  where
  liftGMrgIte s = mrgIfWithStrategy (SimpleStrategy s)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e, Functor m) =>
  GUnionLike bool (ErrorC e m)
  where
  mergeWithStrategy s (ErrorC et) = ErrorC $ mergeWithStrategy s et
  mrgIfWithStrategy s cond (ErrorC l) (ErrorC r) = ErrorC $ mrgIfWithStrategy s cond l r
  mrgSingleWithStrategy s a = ErrorC $ mrgSingleWithStrategy s a
  single a = ErrorC $ single a
  unionIf cond (ErrorC t) (ErrorC f) = ErrorC $ unionIf cond t f

mrgRunError ::
  ( SymBoolOp bool,
    GUnionLike bool m,
    GMergeable bool exc,
    GMergeable bool a
  ) =>
  ErrorC exc m a ->
  m (Either exc a)
mrgRunError = merge . runError
