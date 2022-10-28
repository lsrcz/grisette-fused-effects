module Grisette.Lib.Control.Effect.FreshTests where

import Control.Carrier.Fresh.Strict
import Control.Carrier.Lift
import Grisette.Core
import Grisette.Lib.Control.Carrier.Fresh.Strict ()
import Grisette.Lib.Control.Carrier.Lift ()
import Grisette.Lib.Control.Effect.Fresh
import Grisette.Lib.Control.Monad
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

freshTests :: TestTree
freshTests =
  testGroup
    "FreshTests"
    [ testGroup
        "Fresh effect"
        [ testCase "mrgFresh" $ do
            let f1 = mrgFresh :: FreshC (LiftC (UnionMBase SBool)) Int
            runM (runFresh 0 f1) @=? mrgReturn (1, 0)
        ]
    ]
