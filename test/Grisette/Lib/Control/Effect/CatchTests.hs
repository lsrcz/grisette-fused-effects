module Grisette.Lib.Control.Effect.CatchTests where

import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Grisette.Core
import Grisette.Lib.Control.Carrier.Error.Either ()
import Grisette.Lib.Control.Carrier.Lift ()
import Grisette.Lib.Control.Effect.Catch
import Grisette.Lib.Control.Monad
import Grisette.TestUtils.SBool
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
