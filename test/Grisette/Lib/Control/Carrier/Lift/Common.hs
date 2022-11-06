{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Lib.Control.Carrier.Lift.Common where

import Grisette.Core

l :: (GUnionLike bool m, SymBoolOp bool) => a -> m a
l = single

lm ::
  forall m bool a.
  (GUnionLike bool m, SymBoolOp bool, GMergeable bool (m a)) =>
  bool ->
  a ->
  a ->
  m a
lm cond v1 v2 = ms cond (l v1) (l v2)
  where
    SimpleStrategy ms = gmergingStrategy :: GMergingStrategy bool (m a)

lm1 ::
  forall m bool a.
  (GUnionLike bool m, SymBoolOp bool, GMergeable bool a) =>
  bool ->
  a ->
  a ->
  m a
lm1 cond v1 v2 = ms cond (l v1) (l v2)
  where
    SimpleStrategy ms = gmergingStrategy1 :: GMergingStrategy bool (m a)

ls ::
  forall m bool a.
  (GUnionLike bool m, SymBoolOp bool, GSimpleMergeable bool (m a)) =>
  bool ->
  a ->
  a ->
  m a
ls cond v1 v2 = gmrgIte cond (l v1) (l v2)

ls1 ::
  forall m bool a.
  (GUnionLike bool m, SymBoolOp bool, GSimpleMergeable bool a) =>
  bool ->
  a ->
  a ->
  m a
ls1 cond v1 v2 = gmrgIte1 cond (l v1) (l v2)

lu ::
  forall m bool a.
  (GUnionLike bool m, SymBoolOp bool, GMergeable bool a) =>
  bool ->
  a ->
  a ->
  m a
lu cond v1 v2 = mrgIf cond (l v1) (l v2)

lu' ::
  forall m bool a.
  (GUnionLike bool m, SymBoolOp bool) =>
  bool ->
  a ->
  a ->
  m a
lu' cond v1 v2 = unionIf cond (l v1) (l v2)
