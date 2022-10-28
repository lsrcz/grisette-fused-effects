module Grisette.Lib.Control.Effect.ReaderTests where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Grisette.Core
import Grisette.Lib.Control.Carrier.Lift ()
import Grisette.Lib.Control.Carrier.Reader ()
import Grisette.Lib.Control.Effect.Reader
import Grisette.Lib.Control.Monad
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

readerTests :: TestTree
readerTests =
  testGroup
    "ReaderTests"
    [ let r1 = mrgAsk :: ReaderC Int (LiftC (UnionMBase SBool)) Int
          r2 = mrgAsks (+ 1) :: ReaderC Int (LiftC (UnionMBase SBool)) Int
          r3 = mrgLocal (+ (1 :: Int)) mrgAsk :: ReaderC Int (LiftC (UnionMBase SBool)) Int
       in testGroup
            "Reader effect"
            [ testCase "mrgAsk" $ do
                runM (runReader 0 r1) @=? mrgReturn 0,
              testCase "mrgAsks" $ do
                runM (runReader 0 r2) @=? mrgReturn 1,
              testCase "mrgLocal" $ do
                runM (runReader 0 r3) @=? mrgReturn 1
                runM (runReader 0 $ r3 >> r1) @=? mrgReturn 0
            ]
    ]
