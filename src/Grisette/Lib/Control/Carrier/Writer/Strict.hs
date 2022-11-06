{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.Writer.Strict where

import Control.Carrier.Writer.Strict
import Grisette.Core
import Grisette.Lib.Control.Carrier.State.Strict

instance
  (SymBoolOp bool, GMergeable1 bool m, GMergeable bool w, GMergeable bool a) =>
  GMergeable bool (WriterC w m a)
  where
  gmergingStrategy = gwrapStrategy gmergingStrategy WriterC (\(WriterC et) -> et)

instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool w, Functor m) => GMergeable1 bool (WriterC w m) where
  liftGMergingStrategy ms = gwrapStrategy (liftGMergingStrategy ms) WriterC (\(WriterC et) -> et)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool w, GMergeable bool a, Functor m) =>
  GSimpleMergeable bool (WriterC w m a)
  where
  gmrgIte cond (WriterC t) (WriterC f) = WriterC $ gmrgIte cond t f

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool w, Functor m) =>
  GSimpleMergeable1 bool (WriterC w m)
  where
  liftGMrgIte s = mrgIfWithStrategy (SimpleStrategy s)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool w, Functor m) =>
  GUnionLike bool (WriterC w m)
  where
  mergeWithStrategy s (WriterC et) = WriterC $ mergeWithStrategy s et
  mrgIfWithStrategy s cond (WriterC l) (WriterC r) = WriterC $ mrgIfWithStrategy s cond l r
  mrgSingleWithStrategy ms a = WriterC $ mrgSingleWithStrategy ms a
  single a = WriterC $ single a
  unionIf cond (WriterC t) (WriterC f) = WriterC $ unionIf cond t f

mrgRunWriter ::
  ( SymBoolOp bool,
    GUnionLike bool m,
    GMergeable bool w,
    GMergeable bool a,
    Monoid w
  ) =>
  WriterC w m a ->
  m (w, a)
mrgRunWriter (WriterC m) = mrgRunState mempty m

mrgExecWriter ::
  ( SymBoolOp bool,
    GUnionLike bool m,
    GMergeable bool w,
    Monoid w,
    Functor m
  ) =>
  WriterC w m a ->
  m w
mrgExecWriter (WriterC m) = mrgExecState mempty m
