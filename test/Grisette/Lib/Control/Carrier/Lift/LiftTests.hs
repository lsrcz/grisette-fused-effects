module Grisette.Lib.Control.Carrier.Lift.LiftTests where

import Control.Carrier.Lift
import Grisette.Core
import Grisette.Lib.Control.Carrier.Lift
import Grisette.Lib.Control.Carrier.Lift.Common
import Grisette.Lib.Control.Monad
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

type M a = LiftC (UnionMBase SBool) a

type R = M Int

type RB = M SBool

liftTests :: TestTree
liftTests =
  testGroup
    "LiftTests"
    [ testGroup
        "Lift"
        [ testCase "Mergeable" $ do
            runM (lm (SSBool "c") 1 2 :: R)
              @=? mrgIf (SSBool "c") (return 1) (return 2)
            runM (lm (SSBool "c") 1 1 :: R)
              @=? mrgReturn 1,
          testCase "Mergeable1" $ do
            runM (lm1 (SSBool "c") 1 2 :: R)
              @=? mrgIf (SSBool "c") (return 1) (return 2)
            runM (lm1 (SSBool "c") 1 1 :: R)
              @=? mrgReturn 1,
          testCase "GSimpleMergeable" $ do
            runM (ls (SSBool "c") 1 2 :: R)
              @=? mrgIf (SSBool "c") (return 1) (return 2)
            runM (ls (SSBool "c") 1 1 :: R)
              @=? mrgReturn 1,
          testCase "GSimpleMergeable1" $ do
            runM (ls (SSBool "c") (SSBool "a") (SSBool "b") :: RB)
              @=? mrgReturn (ITE (SSBool "c") (SSBool "a") (SSBool "b")),
          testCase "GUnionLike" $ do
            runM (lu (SSBool "c") 1 2 :: R)
              @=? mrgIf (SSBool "c") (return 1) (return 2)
            runM (lu (SSBool "c") 1 1 :: R)
              @=? mrgReturn 1
            runM (lu' (SSBool "c") 1 2 :: R)
              @=? unionIf (SSBool "c") (return 1) (return 2)
            runM (lu' (SSBool "c") 1 1 :: R)
              @=? unionIf (SSBool "c") (return 1) (return 1)
            runM (mrgSingle 1 :: R) @=? mrgReturn 1
            runM (single 1 :: R) @=? single 1
            runM (merge $ lu' (SSBool "c") 1 2 :: R)
              @=? mrgIf (SSBool "c") (return 1) (return 2)
            runM (merge $ lu' (SSBool "c") 1 1 :: R)
              @=? mrgReturn 1
            runM (l 1 :: R) @=? single 1,
          testCase "mrgRunM" $ do
            mrgRunM (l 1 :: R) @=? mrgReturn 1
            mrgRunM (single 1 :: R) @=? mrgReturn 1
        ]
    ]
