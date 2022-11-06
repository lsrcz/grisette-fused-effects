{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.Error.Church (mrgRunError) where

import Control.Carrier.Error.Church
import Grisette.Core

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GMergeable bool (ErrorC e m a)
  where
  gmergingStrategy = SimpleStrategy $ \cond (ErrorC l) (ErrorC r) ->
    ErrorC $ \ef af -> unionIf cond (l ef af) (r ef af)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GMergeable1 bool (ErrorC e m)
  where
  liftGMergingStrategy _ = SimpleStrategy $ \cond (ErrorC l) (ErrorC r) ->
    ErrorC $ \ef af -> unionIf cond (l ef af) (r ef af)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GSimpleMergeable bool (ErrorC e m a)
  where
  gmrgIte bool (ErrorC l) (ErrorC r) = ErrorC $ \ef af ->
    unionIf bool (l ef af) (r ef af)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GSimpleMergeable1 bool (ErrorC e m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GUnionLike bool (ErrorC e m)
  where
  mergeWithStrategy _ = id
  mrgIfWithStrategy _ = unionIf
  mrgSingleWithStrategy _ = single
  single a = ErrorC $ \_ leaf -> leaf a
  unionIf cond (ErrorC l) (ErrorC r) =
    ErrorC $ \ef af -> unionIf cond (l ef af) (r ef af)

mrgRunError ::
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool b) =>
  (e -> m b) ->
  (a -> m b) ->
  ErrorC e m a ->
  m b
mrgRunError f leaf = runError (merge . f) (merge . leaf)
