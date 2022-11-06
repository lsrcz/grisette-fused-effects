{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.Fail.Either where

import Control.Carrier.Fail.Either
import Grisette.Core
import Grisette.Lib.Control.Carrier.Throw.Either ()

instance
  (SymBoolOp bool, GMergeable1 bool m, GMergeable bool a) =>
  GMergeable bool (FailC m a)
  where
  gmergingStrategy = gwrapStrategy gmergingStrategy FailC (\(FailC et) -> et)

instance (SymBoolOp bool, GMergeable1 bool m, Functor m) => GMergeable1 bool (FailC m) where
  liftGMergingStrategy ms = gwrapStrategy (liftGMergingStrategy ms) FailC (\(FailC et) -> et)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool a, Functor m) =>
  GSimpleMergeable bool (FailC m a)
  where
  gmrgIte cond (FailC t) (FailC f) = FailC $ gmrgIte cond t f

instance
  (SymBoolOp bool, GUnionLike bool m, Functor m) =>
  GSimpleMergeable1 bool (FailC m)
  where
  liftGMrgIte s = mrgIfWithStrategy (SimpleStrategy s)

instance
  (SymBoolOp bool, GUnionLike bool m, Functor m) =>
  GUnionLike bool (FailC m)
  where
  mergeWithStrategy s (FailC et) = FailC $ mergeWithStrategy s et
  mrgIfWithStrategy s cond (FailC l) (FailC r) = FailC $ mrgIfWithStrategy s cond l r
  mrgSingleWithStrategy s a = FailC $ mrgSingleWithStrategy s a
  single a = FailC $ single a
  unionIf cond (FailC t) (FailC f) = FailC $ unionIf cond t f

mrgRunFail ::
  ( SymBoolOp bool,
    GUnionLike bool m,
    GMergeable bool a
  ) =>
  FailC m a ->
  m (Either String a)
mrgRunFail = merge . runFail
