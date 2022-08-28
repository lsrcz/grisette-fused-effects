module Pizza.Lib.Control.Effect.Fresh where

import Control.Effect.Fresh
import Pizza.Core

mrgFresh :: (Has Fresh sig m, SymBoolOp bool, UnionLike bool m) => m Int
mrgFresh = merge fresh
