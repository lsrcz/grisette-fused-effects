module Pizza.Lib.Control.Carrier.State.LazyTests where

import Control.Carrier.Lift
import Control.Carrier.State.Lazy
import Pizza.Core
import Pizza.Lib.Control.Carrier.Lift ()
import Pizza.Lib.Control.Carrier.State.Common
import Pizza.Lib.Control.Carrier.State.Lazy
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

type R = StateC Int (LiftC (UnionMBase SBool)) SBool

lazyStateTests :: TestTree
lazyStateTests =
  testGroup
    "LazyTests"
    [ let r0 = mrgIf (SSBool "c") (return (2, SSBool "a")) (return (0, SSBool "b"))
          r2 = mrgReturn (4, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
          r0' = unionIf (SSBool "c") (return (2, SSBool "a")) (return (0, SSBool "b"))
          r2' = unionIf (SSBool "c") (return (4, SSBool "a")) (return (4, SSBool "b"))
       in testGroup
            "State.Lazy"
            [ testCase "Mergeable" $ do
                let s1 = sm (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState 0 s1) @=? r0
                runM (runState 2 s1) @=? r2,
              testCase "Mergeable1" $ do
                let s1 = sm1 (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState 0 s1) @=? r0
                runM (runState 2 s1) @=? r2,
              testCase "SimpleMergeable" $ do
                let s1 = ss (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState 0 s1) @=? r0
                runM (runState 2 s1) @=? r2,
              testCase "SimpleMergeable1" $ do
                let s1 = ss1 (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState 0 s1) @=? r0
                runM (runState 2 s1) @=? r2,
              testCase "UnionLike" $ do
                let s1 = su (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState 0 s1) @=? r0
                runM (runState 2 s1) @=? r2
                let s1' = su' (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState 0 s1') @=? r0'
                runM (runState 2 s1') @=? r2'
                runM (runState 0 $ merge s1') @=? r0
                runM (runState 2 $ merge s1') @=? r2
                runM (runState 0 (mrgSingle (SSBool "a") :: R)) @=? mrgReturn (0, SSBool "a")
                runM (runState 0 (single (SSBool "a") :: R)) @=? return (0, SSBool "a"),
              testCase "mrgRunState" $ do
                let s1' = su' (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (mrgRunState 0 s1') @=? r0
                runM (mrgRunState 2 s1') @=? r2,
              testCase "mrgExecState" $ do
                let s1' = su' (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (mrgExecState 0 s1') @=? mrgIf (SSBool "c") (return 2) (return 0)
                runM (mrgExecState 2 s1') @=? mrgReturn 4,
              testCase "mrgEvalState" $ do
                let s1' = su' (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (mrgEvalState 0 s1') @=? mrgIf (SSBool "c") (return $ SSBool "a") (return $ SSBool "b")
                runM (mrgEvalState 2 s1') @=? mrgIf (SSBool "c") (return $ SSBool "a") (return $ SSBool "b")
            ]
    ]
