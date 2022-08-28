module Pizza.Lib.Control.Effect.CatchTests where

import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Pizza.Core
import Pizza.Lib.Control.Carrier.Error.Either ()
import Pizza.Lib.Control.Carrier.Lift ()
import Pizza.Lib.Control.Effect.Catch
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

catchTests :: TestTree
catchTests =
  testGroup
    "CatchTests"
    [ testGroup
        "Catch effect"
        [ testCase "mrgCatchError" $ do
            let e1 = throwError (1 :: Int) `mrgCatchError` (\x -> return $ x + 1) :: ErrorC Int (LiftC (UnionMBase SBool)) Int
            runM (runError e1) @=? mrgReturn (Right 2)
        ]
    ]
