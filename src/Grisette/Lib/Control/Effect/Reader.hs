module Grisette.Lib.Control.Effect.Reader where

import Control.Effect.Reader
import Grisette.Core

mrgAsk :: (Has (Reader r) sig m, SymBoolOp bool, GUnionLike bool m, GMergeable bool r) => m r
mrgAsk = merge ask

mrgAsks :: (Has (Reader r) sig m, SymBoolOp bool, GUnionLike bool m, GMergeable bool a) => (r -> a) -> m a
mrgAsks = merge . asks

mrgLocal :: (Has (Reader r) sig m, SymBoolOp bool, GUnionLike bool m, GMergeable bool a) => (r -> r) -> m a -> m a
mrgLocal f = merge . local f
