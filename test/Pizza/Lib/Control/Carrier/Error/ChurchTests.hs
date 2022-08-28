module Pizza.Lib.Control.Carrier.Error.ChurchTests where

import Control.Carrier.Error.Church
import Control.Carrier.Lift
import Pizza.Core
import Pizza.Lib.Control.Carrier.Error.Church
import Pizza.Lib.Control.Carrier.Error.Common
import Pizza.Lib.Control.Carrier.Lift ()
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

type M a = ErrorC Int (LiftC (UnionMBase SBool)) a

type R = M Int

type RB = M SBool

churchErrorTests :: TestTree
churchErrorTests =
  testGroup
    "ChurchTests"
    [ testGroup
        "Error.Church"
        [ testCase "Mergeable" $ do
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (em (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? mrgReturn (Left 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (em (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (em (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (em (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? mrgReturn (Right 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (em (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? mrgIf (SSBool "c") (return (Right 1)) (return (Right 2)),
          testCase "Mergeable1" $ do
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (em1 (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? mrgReturn (Left 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (em1 (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (em1 (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (em1 (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? mrgReturn (Right 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (em1 (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? mrgIf (SSBool "c") (return (Right 1)) (return (Right 2)),
          testCase "SimpleMergeable" $ do
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (es (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? mrgReturn (Left 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (es (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (es (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (es (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? mrgReturn (Right 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (es (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? mrgIf (SSBool "c") (return (Right 1)) (return (Right 2)),
          testCase "SimpleMergeable1" $ do
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (es1 (SSBool "c") (Left (1 :: Int)) (Left 1) :: RB))
              @=? mrgReturn (Left 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (es1 (SSBool "c") (Left (1 :: Int)) (Left 2) :: RB))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (es1 (SSBool "c") (Left (1 :: Int)) (Right $ SSBool "a") :: RB))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right $ SSBool "a"))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (es1 (SSBool "c") (Right $ SSBool "a" :: Either Int SBool) (Right $ SSBool "b") :: RB))
              @=? mrgReturn (Right $ ITE (SSBool "c") (SSBool "a") (SSBool "b")),
          testCase "UnionLike" $ do
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (eu (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? mrgReturn (Left 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (eu (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (eu (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (eu (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? mrgReturn (Right 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (eu (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? mrgIf (SSBool "c") (return (Right 1)) (return (Right 2))

            runM (runError (mrgReturn . Left) (mrgReturn . Right) (eu' (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? mrgReturn (Left 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (eu' (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (eu' (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (eu' (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? mrgReturn (Right 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (eu' (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? mrgIf (SSBool "c") (return (Right 1)) (return (Right 2))
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (single 1 :: R)) @=? mrgReturn (return 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (mrgSingle 1 :: R)) @=? mrgReturn (return 1)
            runM (runError (mrgReturn . Left) (mrgReturn . Right) (merge $ single 1 :: R)) @=? mrgReturn (return 1),
          testCase "mrgRunError" $ do
            runM (mrgRunError (return . Left) (return . Right) (eu' (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? mrgReturn (Left 1)
            runM (mrgRunError (return . Left) (return . Right) (eu' (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (mrgRunError (return . Left) (return . Right) (eu' (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (mrgRunError (return . Left) (return . Right) (eu' (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? mrgReturn (Right 1)
            runM (mrgRunError (return . Left) (return . Right) (eu' (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? mrgIf (SSBool "c") (return (Right 1)) (return (Right 2))
        ]
    ]
