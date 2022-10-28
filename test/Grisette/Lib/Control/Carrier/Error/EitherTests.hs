module Grisette.Lib.Control.Carrier.Error.EitherTests where

import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Grisette.Core
import Grisette.Lib.Control.Carrier.Error.Common
import Grisette.Lib.Control.Carrier.Error.Either
import Grisette.Lib.Control.Carrier.Lift ()
import Grisette.Lib.Control.Monad
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

type M a = ErrorC Int (LiftC (UnionMBase SBool)) a

type R = M Int

type RB = M SBool

eitherErrorTests :: TestTree
eitherErrorTests =
  testGroup
    "EitherTests"
    [ testGroup
        "Error.Either"
        [ testCase "Mergeable" $ do
            runM (runError (em (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? mrgReturn (Left 1)
            runM (runError (em (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (em (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (runError (em (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? mrgReturn (Right 1)
            runM (runError (em (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? mrgIf (SSBool "c") (return (Right 1)) (return (Right 2)),
          testCase "Mergeable1" $ do
            runM (runError (em1 (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? mrgReturn (Left 1)
            runM (runError (em1 (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (em1 (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (runError (em1 (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? mrgReturn (Right 1)
            runM (runError (em1 (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? mrgIf (SSBool "c") (return (Right 1)) (return (Right 2)),
          testCase "SimpleMergeable" $ do
            runM (runError (es (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? mrgReturn (Left 1)
            runM (runError (es (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (es (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (runError (es (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? mrgReturn (Right 1)
            runM (runError (es (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? mrgIf (SSBool "c") (return (Right 1)) (return (Right 2)),
          testCase "SimpleMergeable1" $ do
            runM (runError (es1 (SSBool "c") (Left (1 :: Int)) (Left 1) :: RB))
              @=? mrgReturn (Left 1)
            runM (runError (es1 (SSBool "c") (Left (1 :: Int)) (Left 2) :: RB))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (es1 (SSBool "c") (Left (1 :: Int)) (Right $ SSBool "a") :: RB))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right $ SSBool "a"))
            runM (runError (es1 (SSBool "c") (Right $ SSBool "a" :: Either Int SBool) (Right $ SSBool "b") :: RB))
              @=? mrgReturn (Right $ ITE (SSBool "c") (SSBool "a") (SSBool "b")),
          testCase "UnionLike" $ do
            runM (runError (eu (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? mrgReturn (Left 1)
            runM (runError (eu (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (eu (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (runError (eu (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? mrgReturn (Right 1)
            runM (runError (eu (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? mrgIf (SSBool "c") (return (Right 1)) (return (Right 2))
            runM (runError (eu' (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? unionIf (SSBool "c") (return (Left 1)) (return (Left 1))
            runM (runError (eu' (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? unionIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (runError (eu' (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? unionIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (runError (eu' (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? unionIf (SSBool "c") (return (Right 1)) (return (Right 1))
            runM (runError (eu' (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? unionIf (SSBool "c") (return (Right 1)) (return (Right 2))
            runM (runError (single 1 :: R)) @=? return (return 1)
            runM (runError (mrgSingle 1 :: R)) @=? mrgReturn (return 1)
            runM (runError (merge $ single 1 :: R)) @=? mrgReturn (return 1),
          testCase "mrgRunError" $ do
            runM (mrgRunError (eu' (SSBool "c") (Left (1 :: Int)) (Left 1) :: R))
              @=? mrgReturn (Left 1)
            runM (mrgRunError (eu' (SSBool "c") (Left (1 :: Int)) (Left 2) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Left 2))
            runM (mrgRunError (eu' (SSBool "c") (Left (1 :: Int)) (Right 1) :: R))
              @=? mrgIf (SSBool "c") (return (Left 1)) (return (Right 1))
            runM (mrgRunError (eu' (SSBool "c") (Right 1 :: Either Int Int) (Right 1) :: R))
              @=? mrgReturn (Right 1)
            runM (mrgRunError (eu' (SSBool "c") (Right 1 :: Either Int Int) (Right 2) :: R))
              @=? mrgIf (SSBool "c") (return (Right 1)) (return (Right 2))
        ]
    ]
