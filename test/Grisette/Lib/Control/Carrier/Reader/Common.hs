{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Lib.Control.Carrier.Reader.Common where

import Control.Effect.Reader
import Grisette.Core

r :: (Has (Reader Int) sig m, GUnionLike bool m, SymBoolOp bool) => (Int -> a) -> m a
r = asks

rm ::
  forall sig m bool a.
  (Has (Reader Int) sig m, GUnionLike bool m, SymBoolOp bool, GMergeable bool (m a)) =>
  bool ->
  (Int -> a) ->
  (Int -> a) ->
  m a
rm cond f1 f2 = ms cond (r f1) (r f2)
  where
    SimpleStrategy ms = gmergingStrategy :: GMergingStrategy bool (m a)

rm1 ::
  forall sig m bool a.
  (Has (Reader Int) sig m, GUnionLike bool m, SymBoolOp bool, GMergeable bool a) =>
  bool ->
  (Int -> a) ->
  (Int -> a) ->
  m a
rm1 cond f1 f2 = ms cond (r f1) (r f2)
  where
    SimpleStrategy ms = gmergingStrategy1 :: GMergingStrategy bool (m a)

rs ::
  forall sig m bool a.
  (Has (Reader Int) sig m, GUnionLike bool m, SymBoolOp bool, GSimpleMergeable bool (m a)) =>
  bool ->
  (Int -> a) ->
  (Int -> a) ->
  m a
rs cond f1 f2 = gmrgIte cond (r f1) (r f2)

rs1 ::
  forall sig m bool a.
  (Has (Reader Int) sig m, GUnionLike bool m, SymBoolOp bool, GSimpleMergeable bool a) =>
  bool ->
  (Int -> a) ->
  (Int -> a) ->
  m a
rs1 cond f1 f2 = gmrgIte1 cond (r f1) (r f2)

ru ::
  forall sig m bool a.
  (Has (Reader Int) sig m, GUnionLike bool m, SymBoolOp bool, GMergeable bool a) =>
  bool ->
  (Int -> a) ->
  (Int -> a) ->
  m a
ru cond f1 f2 = mrgIf cond (r f1) (r f2)

ru' ::
  forall sig m bool a.
  (Has (Reader Int) sig m, GUnionLike bool m, SymBoolOp bool) =>
  bool ->
  (Int -> a) ->
  (Int -> a) ->
  m a
ru' cond f1 f2 = unionIf cond (r f1) (r f2)
