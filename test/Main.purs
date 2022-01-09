module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Test.Parser as Test.Parser
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = Aff.launchAff_ $ runSpec [consoleReporter] Test.Parser.test
