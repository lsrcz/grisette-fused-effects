module Pizza.Lib.Control.Carrier.Writer.ChurchTests where

import Control.Carrier.Lift
import Control.Carrier.Writer.Church
import Data.Monoid
import Pizza.Core
import Pizza.Lib.Control.Carrier.Lift ()
import Pizza.Lib.Control.Carrier.Writer.Church
import Pizza.Lib.Control.Carrier.Writer.Common
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

type R = WriterC (Sum Int) (LiftC (UnionMBase SBool)) SBool

churchWriterTests :: TestTree
churchWriterTests =
  testGroup
    "ChurchTests"
    [ testGroup
        "Writer.Church"
        [ testCase "Mergeable" $ do
            runM (runWriter (curry mrgReturn) (wm (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgReturn (Sum 10, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runM (runWriter (curry mrgReturn) (wm (SSBool "c") 10 (SSBool "a") 20 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (mrgReturn (Sum 10, SSBool "a")) (mrgReturn (Sum 20, SSBool "b")),
          testCase "Mergeable1" $ do
            runM (runWriter (curry mrgReturn) (wm1 (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgReturn (Sum 10, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runM (runWriter (curry mrgReturn) (wm1 (SSBool "c") 10 (SSBool "a") 20 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (mrgReturn (Sum 10, SSBool "a")) (mrgReturn (Sum 20, SSBool "b")),
          testCase "SimpleMergeable" $ do
            runM (runWriter (curry mrgReturn) (ws (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgReturn (Sum 10, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runM (runWriter (curry mrgReturn) (ws (SSBool "c") 10 (SSBool "a") 20 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (mrgReturn (Sum 10, SSBool "a")) (mrgReturn (Sum 20, SSBool "b")),
          testCase "SimpleMergeable1" $ do
            runM (runWriter (curry mrgReturn) (ws1 (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgReturn (Sum 10, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runM (runWriter (curry mrgReturn) (ws1 (SSBool "c") 10 (SSBool "a") 20 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (mrgReturn (Sum 10, SSBool "a")) (mrgReturn (Sum 20, SSBool "b")),
          testCase "UnionLike" $ do
            runM (runWriter (curry mrgReturn) (wu (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgReturn (Sum 10, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runM (runWriter (curry mrgReturn) (wu (SSBool "c") 10 (SSBool "a") 20 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (mrgReturn (Sum 10, SSBool "a")) (mrgReturn (Sum 20, SSBool "b"))
            runM (runWriter (curry mrgReturn) (merge $ single (SSBool "a") :: R)) @=? mrgSingle (Sum 0, SSBool "a")
            runM (runWriter (curry return) (merge $ single (SSBool "a") :: R)) @=? single (Sum 0, SSBool "a")
            runM (runWriter (curry mrgReturn) (mrgSingle (SSBool "a") :: R)) @=? mrgSingle (Sum 0, SSBool "a")
            runM (runWriter (curry return) (mrgSingle (SSBool "a") :: R)) @=? single (Sum 0, SSBool "a")
            runM (runWriter (curry mrgReturn) (single (SSBool "a") :: R)) @=? mrgSingle (Sum 0, SSBool "a")
            runM (runWriter (curry return) (single (SSBool "a") :: R)) @=? single (Sum 0, SSBool "a")
            runM (runWriter (curry mrgReturn) (wu' (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (single (Sum 10, SSBool "a")) (single (Sum 10, SSBool "b"))
            runM (runWriter (curry return) (wu' (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? unionIf (SSBool "c") (single (Sum 10, SSBool "a")) (single (Sum 10, SSBool "b")),
          testCase "mrgRunWriter" $ do
            runM (mrgRunWriter (curry return) (w 1 (SSBool "a") :: R)) @=? mrgReturn (Sum 1, SSBool "a")
            runM (mrgRunWriter (curry return) (wu' (SSBool "c") 10 (SSBool "a") 10 (SSBool "b") :: R))
              @=? mrgIf (SSBool "c") (single (Sum 10, SSBool "a")) (single (Sum 10, SSBool "b")),
          testCase "mrgExecWriter" $ do
            runM (mrgExecWriter (w 1 (SSBool "a") :: R)) @=? mrgReturn (Sum 1)
        ]
    ]
