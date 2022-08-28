module Pizza.Lib.Control.Effect.WriterTests where

import Control.Carrier.Lift
import Control.Carrier.Writer.Strict
import Data.Monoid
import Pizza.Core
import Pizza.Lib.Control.Carrier.Lift ()
import Pizza.Lib.Control.Carrier.Writer.Strict ()
import Pizza.Lib.Control.Effect.Writer
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

writerTests :: TestTree
writerTests =
  testGroup
    "WriterTests"
    [ let w1 = mrgTell (10 :: Sum Int) :: WriterC (Sum Int) (LiftC (UnionMBase SBool)) ()
          w2 = mrgTell (20 :: Sum Int) :: WriterC (Sum Int) (LiftC (UnionMBase SBool)) ()
          w1' = tell (10 :: Sum Int) >> return 20 :: WriterC (Sum Int) (LiftC (UnionMBase SBool)) (Sum Int)
          w2' = tell (20 :: Sum Int) >> return 10 :: WriterC (Sum Int) (LiftC (UnionMBase SBool)) (Sum Int)
       in testGroup
            "Writer effect"
            [ testCase "mrgTell" $ do
                runM (runWriter w1) @=? mrgReturn (10, ())
                runM (runWriter w2) @=? mrgReturn (20, ())
                runM (runWriter $ w1 >> w2) @=? mrgReturn (30, ()),
              testCase "mrgListen" $ do
                runM (runWriter $ mrgListen w1')
                  @=? mrgReturn ((10, (10, 20)) :: (Sum Int, (Sum Int, Sum Int)))
                runM (runWriter $ mrgListen w2')
                  @=? mrgReturn ((20, (20, 10)) :: (Sum Int, (Sum Int, Sum Int)))
                runM (runWriter $ w1' >> mrgListen w2')
                  @=? mrgReturn ((30, (20, 10)) :: (Sum Int, (Sum Int, Sum Int))),
              testCase "mrgListens" $ do
                runM (runWriter $ mrgListens (+ 1) w1')
                  @=? mrgReturn ((10, (11, 20)) :: (Sum Int, (Sum Int, Sum Int)))
                runM (runWriter $ mrgListens (+ 1) w2')
                  @=? mrgReturn ((20, (21, 10)) :: (Sum Int, (Sum Int, Sum Int)))
                runM (runWriter $ w1' >> mrgListens (+ 1) w2')
                  @=? mrgReturn ((30, (21, 10)) :: (Sum Int, (Sum Int, Sum Int))),
              testCase "mrgCensor" $ do
                runM (runWriter $ mrgCensor (+ (1 :: Sum Int)) w1')
                  @=? mrgReturn ((11, 20) :: (Sum Int, Sum Int))
                runM (runWriter $ mrgCensor (+ (1 :: Sum Int)) w2')
                  @=? mrgReturn ((21, 10) :: (Sum Int, Sum Int))
                runM (runWriter $ w1' >> mrgCensor (+ (1 :: Sum Int)) w2')
                  @=? mrgReturn ((31, 10) :: (Sum Int, Sum Int))
            ]
    ]
