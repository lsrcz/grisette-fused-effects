module Pizza.Lib.Control.Effect.Writer where

import Control.Effect.Writer
import Pizza.Core

mrgTell :: (Has (Writer w) sig m, SymBoolOp bool, UnionLike bool m) => w -> m ()
mrgTell = merge . tell

mrgListen ::
  (Has (Writer w) sig m, SymBoolOp bool, UnionLike bool m, Mergeable bool w, Mergeable bool a) =>
  m a ->
  m (w, a)
mrgListen = merge . listen

mrgListens ::
  (Has (Writer w) sig m, SymBoolOp bool, UnionLike bool m, Mergeable bool b, Mergeable bool a) =>
  (w -> b) ->
  m a ->
  m (b, a)
mrgListens f = merge . listens f

mrgCensor ::
  (Has (Writer w) sig m, SymBoolOp bool, UnionLike bool m, Mergeable bool a) =>
  (w -> w) ->
  m a ->
  m a
mrgCensor f = merge . censor f
