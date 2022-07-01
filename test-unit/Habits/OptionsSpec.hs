module Habits.OptionsSpec where

import Habits.Test.Prelude
import Habits.Options (parseCommandPure, Command (CmdStart), defaultPort)
import Options.Applicative.Types (ParserResult(Success))
import Options.Applicative (getParseResult)
import Test.Hspec (context, shouldBe)

spec :: Spec
spec = describe "Options" $ do
  describe "parseCommandPure should" $ do
    context "when parsing start command" $ do
      it "parse port correctly in short and long form" $ do
        getParseResult (parseCommandPure ["start", "-p", "8020"]) `shouldBe` getParseResult (Success (CmdStart 8020))
        getParseResult (parseCommandPure ["start", "--port", "8020"]) `shouldBe` getParseResult (Success (CmdStart 8020))
      it "use default port when no port is given" $ do
        getParseResult (parseCommandPure ["start"]) `shouldBe` getParseResult (Success (CmdStart defaultPort))


