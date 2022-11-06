{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.Lift where

import Control.Carrier.Lift
import Grisette.Core

instance (GUnionLike bool m, GMergeable bool a) => GMergeable bool (LiftC m a) where
  gmergingStrategy = gwrapStrategy gmergingStrategy1 LiftC (\(LiftC m) -> m)

instance (GUnionLike bool m) => GMergeable1 bool (LiftC m) where
  liftGMergingStrategy ms = gwrapStrategy (liftGMergingStrategy ms) LiftC (\(LiftC m) -> m)

instance (GUnionLike bool m, GMergeable bool a) => GSimpleMergeable bool (LiftC m a) where
  gmrgIte = mrgIf

instance (GUnionLike bool m) => GSimpleMergeable1 bool (LiftC m) where
  liftGMrgIte = mrgIfWithStrategy . SimpleStrategy

instance (GUnionLike bool m) => GUnionLike bool (LiftC m) where
  mergeWithStrategy s (LiftC v) = LiftC $ mergeWithStrategy s v
  mrgIfWithStrategy s cond (LiftC l) (LiftC r) = LiftC $ mrgIfWithStrategy s cond l r
  mrgSingleWithStrategy s v = LiftC $ mrgSingleWithStrategy s v
  single x = LiftC $ single x
  unionIf cond (LiftC l) (LiftC r) = LiftC $ unionIf cond l r

mrgRunM :: (GUnionLike bool m, GMergeable bool a) => LiftC m a -> m a
mrgRunM = merge . runM
