{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Habits.Domain.AccountRepoContract where

import GHC.Stack (HasCallStack)
import qualified Habits.Domain.Account as A
import Habits.Domain.AccountId (AccountId)
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import Habits.Domain.AccountRepo.Class
  ( AccountRepo,
  )
import qualified Habits.Domain.AccountRepo.Class as ARC
import Habits.Domain.Email (Email (Email))
import Habits.Test.Prelude
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Test.Hspec
  ( parallel,
  )
import Test.Hspec.Expectations.Lifted
  ( shouldBe,
  )
import Utils
  ( catchAllToFail,
    sampleIO,
    expectError,
  )
import Veins.Test.QuickCheck (propertyRuns)
import Habits.Domain.Account (updateAccount)

mkSpec :: forall m. (HasCallStack, MonadIO m, AccountRepo m) => (m () -> IO ()) -> Spec
mkSpec unlift = parallel $
  describe "AccountRepoContract" $ do
    let embed :: _ => _
        embed = unlift . evalE . catchAllToFail
    describe "add should" $ do
      it "persist the account to the repository" . propertyRuns 4 $ \accountNew -> embed $ S.do
        accountId <- ARC.add accountNew
        acc <- ARC.getById accountId
        S.coerce $ acc `shouldBe` A.fromAccountNew accountNew accountId
    describe "getById should" $ do
      it "fail with AccountNotFound when repository is empty" . embed $ do
        (accountId :: AccountId) <- sampleIO
        ARC.getById accountId
        & expectError @AccountNotFoundError
    describe "getByEmail should" $ do
      let sampleEmail = Email "abc@abc.de"
      it "return Nothing when repo is empty" . embed $ do
        res <- ARC.getByEmail sampleEmail
        res `shouldBe` Nothing
      it "return Just when account with same Email exists" . embed $ S.do
        accountNew <- S.coerce $ sampleIO <&> (\x -> x{email = sampleEmail})
        accountId <- ARC.add accountNew
        res <- ARC.getByEmail sampleEmail
        let expAccount = A.fromAccountNew accountNew accountId
        S.coerce $ res `shouldBe` Just expAccount
      it "return Nothing when only other accounts with other Emails exist" . embed $ S.do
        accountNew <- S.coerce sampleIO
        ARC.add accountNew
        res <- ARC.getByEmail sampleEmail
        S.coerce $ res `shouldBe` Nothing
    describe "update should" $ do
      it "apply the update to the account" . propertyRuns 2 $ \(accountUp) -> embed $ S.do
        accountNew <- S.coerce sampleIO
        accountId <- ARC.add accountNew
        accountOld <- ARC.getById accountId
        ARC.update accountUp accountId
        acc <- ARC.getById accountId
        S.coerce $ acc `shouldBe` (updateAccount accountUp accountOld)
