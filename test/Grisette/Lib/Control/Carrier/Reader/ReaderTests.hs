module Grisette.Lib.Control.Carrier.Reader.ReaderTests where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Grisette.Core
import Grisette.Lib.Control.Carrier.Lift ()
import Grisette.Lib.Control.Carrier.Reader
import Grisette.Lib.Control.Carrier.Reader.Common
import Grisette.Lib.Control.Monad
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

type M a = ReaderC Int (LiftC (UnionMBase SBool)) a

type R = M Int

type RB = M SBool

readerCarrierTests :: TestTree
readerCarrierTests =
  testGroup
    "ReaderTests"
    [ testGroup
        "Reader"
        [ testCase "Mergeable" $ do
            runM (runReader 0 (rm (SSBool "c") (+ 1) (+ 2) :: R))
              @=? mrgIf (SSBool "c") (return 1) (return 2)
            runM (runReader 0 (rm (SSBool "c") (+ 1) (+ 1) :: R))
              @=? mrgReturn 1,
          testCase "Mergeable1" $ do
            runM (runReader 0 (rm1 (SSBool "c") (+ 1) (+ 2) :: R))
              @=? mrgIf (SSBool "c") (return 1) (return 2)
            runM (runReader 0 (rm1 (SSBool "c") (+ 1) (+ 1) :: R))
              @=? mrgReturn 1,
          testCase "SimpleMergeable" $ do
            runM (runReader 0 (rs (SSBool "c") (+ 1) (+ 2) :: R))
              @=? mrgIf (SSBool "c") (return 1) (return 2)
            runM (runReader 0 (rs (SSBool "c") (+ 1) (+ 1) :: R))
              @=? mrgReturn 1,
          testCase "SimpleMergeable1" $ do
            runM (runReader 0 (rs (SSBool "c") (const $ SSBool "a") (const $ SSBool "b") :: RB))
              @=? mrgReturn (ITE (SSBool "c") (SSBool "a") (SSBool "b")),
          testCase "UnionLike" $ do
            runM (runReader 0 (ru (SSBool "c") (+ 1) (+ 2) :: R))
              @=? mrgIf (SSBool "c") (return 1) (return 2)
            runM (runReader 0 (ru (SSBool "c") (+ 1) (+ 1) :: R))
              @=? mrgReturn 1
            runM (runReader 0 (ru' (SSBool "c") (+ 1) (+ 2) :: R))
              @=? unionIf (SSBool "c") (return 1) (return 2)
            runM (runReader 0 (ru' (SSBool "c") (+ 1) (+ 1) :: R))
              @=? unionIf (SSBool "c") (return 1) (return 1)
            runM (runReader 0 (mrgSingle 1 :: R)) @=? mrgReturn 1
            runM (runReader 0 (single 1 :: R)) @=? single 1
            runM (runReader 0 (merge $ ru' (SSBool "c") (+ 1) (+ 2) :: R))
              @=? mrgIf (SSBool "c") (return 1) (return 2)
            runM (runReader 0 (merge $ ru' (SSBool "c") (+ 1) (+ 1) :: R))
              @=? mrgReturn 1
            runM (runReader 0 (r (+ 1) :: R)) @=? single 1,
          testCase "mrgRunReader" $ do
            runM (mrgRunReader 0 (r (+ 1) :: R)) @=? mrgReturn 1
            runM (mrgRunReader 0 (single 1 :: R)) @=? mrgReturn 1
        ]
    ]
