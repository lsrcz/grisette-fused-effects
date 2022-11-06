module Grisette.Lib.Control.Effect.Writer where

import Control.Effect.Writer
import Grisette.Core

mrgTell :: (Has (Writer w) sig m, SymBoolOp bool, GUnionLike bool m) => w -> m ()
mrgTell = merge . tell

mrgListen ::
  (Has (Writer w) sig m, SymBoolOp bool, GUnionLike bool m, GMergeable bool w, GMergeable bool a) =>
  m a ->
  m (w, a)
mrgListen = merge . listen

mrgListens ::
  (Has (Writer w) sig m, SymBoolOp bool, GUnionLike bool m, GMergeable bool b, GMergeable bool a) =>
  (w -> b) ->
  m a ->
  m (b, a)
mrgListens f = merge . listens f

mrgCensor ::
  (Has (Writer w) sig m, SymBoolOp bool, GUnionLike bool m, GMergeable bool a) =>
  (w -> w) ->
  m a ->
  m a
mrgCensor f = merge . censor f
