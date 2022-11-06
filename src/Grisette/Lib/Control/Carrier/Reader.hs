{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.Reader where

import Control.Carrier.Reader
import Grisette.Core

instance
  ( SymBoolOp bool,
    GMergeable1 bool m,
    GMergeable bool a
  ) =>
  GMergeable bool (ReaderC r m a)
  where
  gmergingStrategy = gmergingStrategy1

instance
  ( SymBoolOp bool,
    GMergeable1 bool m
  ) =>
  GMergeable1 bool (ReaderC r m)
  where
  liftGMergingStrategy ms =
    gwrapStrategy (liftGMergingStrategy (liftGMergingStrategy ms)) ReaderC (\(ReaderC rt) -> rt)

instance
  ( SymBoolOp bool,
    GUnionLike bool m,
    GMergeable bool a
  ) =>
  GSimpleMergeable bool (ReaderC r m a)
  where
  gmrgIte = mrgIf

instance
  ( SymBoolOp bool,
    GUnionLike bool m
  ) =>
  GSimpleMergeable1 bool (ReaderC r m)
  where
  liftGMrgIte = mrgIfWithStrategy . SimpleStrategy

instance
  ( SymBoolOp bool,
    GUnionLike bool m
  ) =>
  GUnionLike bool (ReaderC r m)
  where
  mergeWithStrategy ms (ReaderC f) = ReaderC $ \r -> mergeWithStrategy ms (f r)
  mrgIfWithStrategy ms cond (ReaderC l) (ReaderC r) = ReaderC $
    \v -> mrgIfWithStrategy ms cond (l v) (r v)
  mrgSingleWithStrategy ms a = ReaderC $ \_ -> mrgSingleWithStrategy ms a
  single a = ReaderC $ \_ -> single a
  unionIf cond (ReaderC l) (ReaderC r) = ReaderC $ \v -> unionIf cond (l v) (r v)

mrgRunReader ::
  ( SymBoolOp bool,
    GUnionLike bool m,
    GMergeable bool a
  ) =>
  r ->
  ReaderC r m a ->
  m a
mrgRunReader r (ReaderC f) = merge $ f r
