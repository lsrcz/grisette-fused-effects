{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.Trace.Ignoring where

import Control.Carrier.Trace.Ignoring
import Grisette.Core

instance
  (SymBoolOp bool, GMergeable1 bool m, GMergeable bool a, Functor m) =>
  GMergeable bool (TraceC m a)
  where
  gmergingStrategy = gwrapStrategy gmergingStrategy1 TraceC (\(TraceC et) -> et)

instance (SymBoolOp bool, GMergeable1 bool m, Functor m) => GMergeable1 bool (TraceC m) where
  liftGMergingStrategy ms = gwrapStrategy (liftGMergingStrategy ms) TraceC (\(TraceC et) -> et)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool a, Functor m) =>
  GSimpleMergeable bool (TraceC m a)
  where
  gmrgIte cond (TraceC t) (TraceC f) = TraceC $ mrgIf cond t f

instance
  (SymBoolOp bool, GUnionLike bool m, Functor m) =>
  GSimpleMergeable1 bool (TraceC m)
  where
  liftGMrgIte s = mrgIfWithStrategy (SimpleStrategy s)

instance
  (SymBoolOp bool, GUnionLike bool m, Functor m) =>
  GUnionLike bool (TraceC m)
  where
  mergeWithStrategy s (TraceC et) = TraceC $ mergeWithStrategy s et
  mrgIfWithStrategy s cond (TraceC l) (TraceC r) = TraceC $ mrgIfWithStrategy s cond l r
  mrgSingleWithStrategy ms a = TraceC $ mrgSingleWithStrategy ms a
  single a = TraceC $ single a
  unionIf cond (TraceC t) (TraceC f) = TraceC $ unionIf cond t f

mrgRunTrace :: (SymBoolOp bool, GUnionLike bool m, GMergeable bool a) => TraceC m a -> m a
mrgRunTrace = merge . runTrace
