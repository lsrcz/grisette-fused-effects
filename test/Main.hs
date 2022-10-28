module Main where

import Grisette.Lib.Control.Carrier.Error.ChurchTests
import Grisette.Lib.Control.Carrier.Error.EitherTests
import Grisette.Lib.Control.Carrier.Lift.LiftTests
import Grisette.Lib.Control.Carrier.Reader.ReaderTests
import Grisette.Lib.Control.Carrier.State.ChurchTests
import Grisette.Lib.Control.Carrier.State.LazyTests
import Grisette.Lib.Control.Carrier.State.StrictTests
import Grisette.Lib.Control.Carrier.Throw.EitherTests
import Grisette.Lib.Control.Carrier.Writer.ChurchTests
import Grisette.Lib.Control.Carrier.Writer.StrictTests
import Grisette.Lib.Control.Effect.CatchTests
import Grisette.Lib.Control.Effect.FreshTests
import Grisette.Lib.Control.Effect.ReaderTests
import Grisette.Lib.Control.Effect.StateTests
import Grisette.Lib.Control.Effect.ThrowTests
import Grisette.Lib.Control.Effect.WriterTests
import Test.Tasty
import Test.Tasty.Ingredients
import qualified Test.Tasty.Ingredients.ConsoleReporter as ConsoleReporter
import qualified Test.Tasty.Runners.Reporter as Reporter

main :: IO ()
main = defaultMainWithIngredients [composeReporters Reporter.ingredient ConsoleReporter.consoleTestReporter] tests

tests :: TestTree
tests =
  testGroup
    "grisette-fused-effects"
    [ testGroup
        "Grisette.Lib.Control"
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
