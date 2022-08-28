module Pizza.Lib.Control.Effect.ThrowTests where

import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Pizza.Core
import Pizza.Lib.Control.Carrier.Error.Either ()
import Pizza.Lib.Control.Carrier.Lift ()
import Pizza.Lib.Control.Effect.Throw
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

throwTests :: TestTree
throwTests =
  testGroup
    "ThrowTests"
    [ testGroup
        "Throw effect"
        [ testCase "mrgThrowError" $ do
            let e1 = mrgThrowError (1 :: Int) :: ErrorC Int (LiftC (UnionMBase SBool)) Int
            runM (runError e1) @=? mrgReturn (Left 1),
          testCase "mrgLiftEither" $ do
            let e1 = mrgLiftEither (Left (1 :: Int)) :: ErrorC Int (LiftC (UnionMBase SBool)) Int
            let e2 = mrgLiftEither (Right 1 :: Either Int Int) :: ErrorC Int (LiftC (UnionMBase SBool)) Int
            runM (runError e1) @=? mrgReturn (Left 1)
            runM (runError e2) @=? mrgReturn (Right 1)
        ]
    ]
