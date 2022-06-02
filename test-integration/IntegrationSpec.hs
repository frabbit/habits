{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=IntegrationSpec #-}
module IntegrationSpec where

import qualified IntegrationSpec
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig IntegrationSpec.spec
