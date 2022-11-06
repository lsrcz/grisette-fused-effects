{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.Writer.Church where

import Control.Carrier.Writer.Church
import Grisette.Core
import Grisette.Lib.Control.Carrier.State.Church

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GMergeable bool (WriterC w m a)
  where
  gmergingStrategy = gwrapStrategy gmergingStrategy WriterC (\(WriterC et) -> et)

instance (SymBoolOp bool, GUnionLike bool m) => GMergeable1 bool (WriterC w m) where
  liftGMergingStrategy ms = gwrapStrategy (liftGMergingStrategy ms) WriterC (\(WriterC et) -> et)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GSimpleMergeable bool (WriterC w m a)
  where
  gmrgIte cond (WriterC t) (WriterC f) = WriterC $ gmrgIte cond t f

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GSimpleMergeable1 bool (WriterC w m)
  where
  liftGMrgIte s = mrgIfWithStrategy (SimpleStrategy s)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GUnionLike bool (WriterC w m)
  where
  mergeWithStrategy ms (WriterC et) = WriterC $ mergeWithStrategy ms et
  mrgIfWithStrategy ms cond (WriterC l) (WriterC r) = WriterC $ mrgIfWithStrategy ms cond l r
  mrgSingleWithStrategy ms a = WriterC $ mrgSingleWithStrategy ms a
  single a = WriterC $ single a
  unionIf cond (WriterC t) (WriterC f) = WriterC $ unionIf cond t f

mrgRunWriter ::
  ( SymBoolOp bool,
    GUnionLike bool m,
    GMergeable bool b,
    Monoid w
  ) =>
  (w -> a -> m b) ->
  WriterC w m a ->
  m b
mrgRunWriter c (WriterC m) = mrgRunState c mempty m

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
