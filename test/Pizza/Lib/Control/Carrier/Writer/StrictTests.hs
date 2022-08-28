module Pizza.Lib.Control.Carrier.Writer.StrictTests where

import Control.Carrier.Lift
import Control.Carrier.Writer.Strict
import Data.Monoid
import Pizza.Core
import Pizza.Lib.Control.Carrier.Lift ()
import Pizza.Lib.Control.Carrier.Writer.Common
import Pizza.Lib.Control.Carrier.Writer.Strict
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

type R = WriterC (Sum Int) (LiftC (UnionMBase SBool)) SBool

strictWriterTests :: TestTree
strictWriterTests =
  testGroup
    "StrictTests"
    [ testGroup
        "Writer.Strict"
        [ testCase "Mergeable" $ do
            runM (runWriter (wm (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgReturn (Sum 10, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runM (runWriter (wm (SSBool "c") 10 (SSBool "a") 20 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (mrgReturn (Sum 10, SSBool "a")) (mrgReturn (Sum 20, SSBool "b")),
          testCase "Mergeable1" $ do
            runM (runWriter (wm1 (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgReturn (Sum 10, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runM (runWriter (wm1 (SSBool "c") 10 (SSBool "a") 20 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (mrgReturn (Sum 10, SSBool "a")) (mrgReturn (Sum 20, SSBool "b")),
          testCase "SimpleMergeable" $ do
            runM (runWriter (ws (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgReturn (Sum 10, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runM (runWriter (ws (SSBool "c") 10 (SSBool "a") 20 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (mrgReturn (Sum 10, SSBool "a")) (mrgReturn (Sum 20, SSBool "b")),
          testCase "SimpleMergeable1" $ do
            runM (runWriter (ws1 (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgReturn (Sum 10, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runM (runWriter (ws1 (SSBool "c") 10 (SSBool "a") 20 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (mrgReturn (Sum 10, SSBool "a")) (mrgReturn (Sum 20, SSBool "b")),
          testCase "UnionLike" $ do
            runM (runWriter (wu (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgReturn (Sum 10, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runM (runWriter (wu (SSBool "c") 10 (SSBool "a") 20 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (mrgReturn (Sum 10, SSBool "a")) (mrgReturn (Sum 20, SSBool "b"))
            runM (runWriter (merge $ single (SSBool "a") :: R)) @=? mrgSingle (Sum 0, SSBool "a")
            runM (runWriter (mrgSingle (SSBool "a") :: R)) @=? mrgSingle (Sum 0, SSBool "a")
            runM (runWriter (single (SSBool "a") :: R)) @=? single (Sum 0, SSBool "a")
            runM (runWriter (wu' (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? unionIf (SSBool "c") (single (Sum 10, SSBool "a")) (single (Sum 10, SSBool "b")),
          testCase "mrgRunWriter" $ do
            runM (mrgRunWriter (w 1 (SSBool "a") :: R)) @=? mrgReturn (Sum 1, SSBool "a")
            runM (mrgRunWriter (wu' (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (single (Sum 10, SSBool "a")) (single (Sum 10, SSBool "b")),
          testCase "mrgExecWriter" $ do
            runM (mrgExecWriter (w 1 (SSBool "a") :: R)) @=? mrgReturn (Sum 1)
        ]
    ]
