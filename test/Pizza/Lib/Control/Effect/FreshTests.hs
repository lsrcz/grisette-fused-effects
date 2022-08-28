module Pizza.Lib.Control.Effect.FreshTests where

import Control.Carrier.Fresh.Strict
import Control.Carrier.Lift
import Pizza.Core
import Pizza.Lib.Control.Carrier.Fresh.Strict ()
import Pizza.Lib.Control.Carrier.Lift ()
import Pizza.Lib.Control.Effect.Fresh
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
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
