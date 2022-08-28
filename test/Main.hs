module Main where

import Pizza.Lib.Control.Carrier.Error.ChurchTests
import Pizza.Lib.Control.Carrier.Error.EitherTests
import Pizza.Lib.Control.Carrier.Lift.LiftTests
import Pizza.Lib.Control.Carrier.Reader.ReaderTests
import Pizza.Lib.Control.Carrier.State.ChurchTests
import Pizza.Lib.Control.Carrier.State.LazyTests
import Pizza.Lib.Control.Carrier.State.StrictTests
import Pizza.Lib.Control.Carrier.Throw.EitherTests
import Pizza.Lib.Control.Carrier.Writer.ChurchTests
import Pizza.Lib.Control.Carrier.Writer.StrictTests
import Pizza.Lib.Control.Effect.CatchTests
import Pizza.Lib.Control.Effect.FreshTests
import Pizza.Lib.Control.Effect.ReaderTests
import Pizza.Lib.Control.Effect.StateTests
import Pizza.Lib.Control.Effect.ThrowTests
import Pizza.Lib.Control.Effect.WriterTests
import Test.Tasty
import Test.Tasty.Ingredients
import qualified Test.Tasty.Ingredients.ConsoleReporter as ConsoleReporter
import qualified Test.Tasty.Runners.Reporter as Reporter

main :: IO ()
main = defaultMainWithIngredients [composeReporters Reporter.ingredient ConsoleReporter.consoleTestReporter] tests

tests :: TestTree
tests =
  testGroup
    "pizza-fused-effects"
    [ testGroup
        "Pizza.Lib.Control"
        [ testGroup
            "Carrier"
            [ testGroup
                "Error"
                [ churchErrorTests,
                  eitherErrorTests
                ],
              testGroup
                "Lift"
                [liftTests],
              testGroup
                "Reader"
                [readerCarrierTests],
              testGroup
                "State"
                [ churchStateTests,
                  lazyStateTests,
                  strictStateTests
                ],
              testGroup
                "Throw"
                [eitherThrowTests],
              testGroup
                "Writer"
                [ churchWriterTests,
                  strictWriterTests
                ]
            ],
          testGroup
            "Effect"
            [ catchTests,
              freshTests,
              readerTests,
              stateTests,
              throwTests,
              writerTests
            ]
        ]
    ]
