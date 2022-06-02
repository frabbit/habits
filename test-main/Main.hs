module Main where

import Prelude

import qualified UnitSpec
import qualified IntegrationSpec
import qualified E2ESpec
import           Test.Hspec.Runner
import           Test.Hspec (describe, Spec)

spec :: Spec
spec = describe "All Tests" $ do
  describe "Unit" UnitSpec.spec
  describe "Integration" IntegrationSpec.spec
  describe "E2E" E2ESpec.spec

main :: IO ()
main = hspecWith defaultConfig spec
