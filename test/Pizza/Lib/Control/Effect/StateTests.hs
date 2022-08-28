{-# LANGUAGE TypeApplications #-}

module Pizza.Lib.Control.Effect.StateTests where

import Control.Carrier.Lift
import Control.Carrier.State.Strict
import Pizza.Core
import Pizza.Lib.Control.Carrier.Lift ()
import Pizza.Lib.Control.Carrier.State.Strict ()
import Pizza.Lib.Control.Effect.State
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

stateTests :: TestTree
stateTests =
  testGroup
    "StateTests"
    [ let s1 = mrgState (\a -> (a * 2 :: Int, a + 2)) :: StateC Int (LiftC (UnionMBase SBool)) Int
          s2 = mrgState (\a -> (a + 2 :: Int, a * 2)) :: StateC Int (LiftC (UnionMBase SBool)) Int
       in testGroup
            "State effect"
            [ testCase "mrgState" $ do
                runM (runState 0 s1) @=? mrgReturn (0, 2)
                runM (runState 0 s2) @=? mrgReturn (2, 0)
                runM (runState 2 s1) @=? mrgReturn (4, 4)
                runM (runState 2 s2) @=? mrgReturn (4, 4)
                runM (runState 0 $ s1 >> s2) @=? mrgReturn (2, 0)
                runM (runState 2 $ s1 >> s2) @=? mrgReturn (6, 8)
                runM (runState 0 $ s2 >> s1) @=? mrgReturn (4, 4)
                runM (runState 2 $ s2 >> s1) @=? mrgReturn (8, 6),
              testCase "mrgGet" $ do
                runM (runState 0 $ s1 >> mrgGet @Int) @=? mrgReturn (0, 0)
                runM (runState 2 $ s1 >> mrgGet @Int) @=? mrgReturn (4, 4)
                runM (runState 0 $ s2 >> mrgGet @Int) @=? mrgReturn (2, 2)
                runM (runState 2 $ s2 >> mrgGet @Int) @=? mrgReturn (4, 4),
              testCase "mrgGets" $ do
                runM (runState 0 $ s1 >> mrgGets @Int (+ 1)) @=? mrgReturn (0, 1)
                runM (runState 2 $ s1 >> mrgGets @Int (+ 1)) @=? mrgReturn (4, 5)
                runM (runState 0 $ s2 >> mrgGets @Int (+ 1)) @=? mrgReturn (2, 3)
                runM (runState 2 $ s2 >> mrgGets @Int (+ 1)) @=? mrgReturn (4, 5),
              testCase "mrgPut" $ do
                runM (runState 0 $ s1 >> mrgPut @Int 1) @=? mrgReturn (1, ())
                runM (runState 2 $ s1 >> mrgPut @Int 1) @=? mrgReturn (1, ())
                runM (runState 0 $ mrgPut @Int 1 >> s1) @=? mrgReturn (2, 3)
                runM (runState 2 $ mrgPut @Int 1 >> s1) @=? mrgReturn (2, 3),
              testCase "mrgModify" $ do
                runM (runState 0 $ s1 >> mrgModify @Int (+ 1)) @=? mrgReturn (1, ())
                runM (runState 2 $ s1 >> mrgModify @Int (+ 1)) @=? mrgReturn (5, ())
                runM (runState 0 $ mrgModify @Int (+ 1) >> s1) @=? mrgReturn (2, 3)
                runM (runState 2 $ mrgModify @Int (+ 1) >> s1) @=? mrgReturn (6, 5),
              testCase "mrgModifyLazy" $ do
                runM (runState 0 $ s1 >> mrgModifyLazy @Int (+ 1)) @=? mrgReturn (1, ())
                runM (runState 2 $ s1 >> mrgModifyLazy @Int (+ 1)) @=? mrgReturn (5, ())
                runM (runState 0 $ mrgModifyLazy @Int (+ 1) >> s1) @=? mrgReturn (2, 3)
                runM (runState 2 $ mrgModifyLazy @Int (+ 1) >> s1) @=? mrgReturn (6, 5)
            ]
    ]
