module Grisette.Lib.Control.Carrier.State.ChurchTests where

import Control.Carrier.Lift
import Control.Carrier.State.Church
import Grisette.Core
import Grisette.Lib.Control.Carrier.Lift ()
import Grisette.Lib.Control.Carrier.State.Church
import Grisette.Lib.Control.Carrier.State.Common
import Grisette.Lib.Control.Monad
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

type R = StateC Int (LiftC (UnionMBase SBool)) SBool

churchStateTests :: TestTree
churchStateTests =
  testGroup
    "ChurchTests"
    [ let r0 = mrgIf (SSBool "c") (return (2, SSBool "a")) (return (0, SSBool "b"))
          r2 = mrgReturn (4, ITE (SSBool "c") (SSBool "a") (SSBool "b"))
       in testGroup
            "State.Church"
            [ testCase "Mergeable" $ do
                let s1 = sm (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState (curry mrgReturn) 0 s1) @=? r0
                runM (runState (curry mrgReturn) 2 s1) @=? r2,
              testCase "Mergeable1" $ do
                let s1 = sm1 (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState (curry mrgReturn) 0 s1) @=? r0
                runM (runState (curry mrgReturn) 2 s1) @=? r2,
              testCase "SimpleMergeable" $ do
                let s1 = ss (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState (curry mrgReturn) 0 s1) @=? r0
                runM (runState (curry mrgReturn) 2 s1) @=? r2,
              testCase "SimpleMergeable1" $ do
                let s1 = ss1 (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState (curry mrgReturn) 0 s1) @=? r0
                runM (runState (curry mrgReturn) 2 s1) @=? r2,
              testCase "UnionLike" $ do
                let s1 = su (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState (curry mrgReturn) 0 s1) @=? r0
                runM (runState (curry mrgReturn) 2 s1) @=? r2
                let s1' = su' (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (runState (curry mrgReturn) 0 s1') @=? r0
                runM (runState (curry mrgReturn) 2 s1') @=? r2
                runM (runState (curry mrgReturn) 0 $ merge s1') @=? r0
                runM (runState (curry mrgReturn) 2 $ merge s1') @=? r2
                runM (runState (curry mrgReturn) 0 (mrgSingle (SSBool "a") :: R)) @=? mrgReturn (0, SSBool "a")
                runM (runState (curry mrgReturn) 0 (single (SSBool "a") :: R)) @=? mrgReturn (0, SSBool "a"),
              testCase "mrgRunState" $ do
                let s1' = su' (SSBool "c") (+ 2) (const $ SSBool "a") (* 2) (const $ SSBool "b") :: R
                runM (mrgRunState (curry mrgReturn) 0 s1') @=? r0
                runM (mrgRunState (curry mrgReturn) 2 s1') @=? r2,
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
