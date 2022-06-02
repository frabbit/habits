{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Habits.Domain.AccountRepositoryContract where

import Habits.Test.Prelude

import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import qualified Habits.Domain.Account as A
import Habits.Domain.AccountId (AccountId)
import qualified Habits.Domain.AccountId as AccountId
import Habits.Domain.AccountNotFoundError (AccountNotFoundError(AccountNotFoundError))
import Habits.Domain.RepositoryError (RepositoryError)
import Habits.Domain.AccountRepo.Class
  ( AccountRepo,
  )
import qualified Habits.Domain.AccountRepo.Class as ARC
import Habits.Domain.Email (Email (Email))
import Haskus.Utils.Variant.Excepts (catchLiftLeft)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Test.Hspec
  ( parallel,
  )
import Test.Hspec.Expectations.Lifted
  ( expectationFailure,
    shouldBe,
  )
import Utils
  ( catchAllToFail,
    sampleIO,
    toThrow,
  )

mkSpec :: forall m. (HasCallStack, MonadIO m, AccountRepo m) => (m () -> IO ()) -> Spec
mkSpec unlift = parallel $
  describe "AccountRepositoryContract" $ do
    let embed = unlift . evalE
    describe "add should" $ do
      it "persist the account to the repository" $
        embed $
          S.do
            accountNew <- S.coerce sampleIO
            accountId <- ARC.add accountNew
            acc <- ARC.getById accountId
            S.coerce $ acc `shouldBe` A.fromAccountNew accountNew accountId
            S.coerce $ ((Text.length . AccountId.unwrap $ accountId) > 0) `shouldBe` True
            & toThrow @RepositoryError
            & toThrow @AccountNotFoundError
    describe "getById should" $ do
      it "fail with AccountNotFound when repository is empty" $
        embed $
          do
            (accountId :: AccountId) <- sampleIO
            res <-
              (ARC.getById accountId & fmap Right)
                & catchLiftLeft (\(e :: AccountNotFoundError) -> pure . Left $ e)
            case res of
              Left e -> e `shouldBe` AccountNotFoundError
              Right _ -> expectationFailure "AccountNotFoundError expected"

            ((Text.length . AccountId.unwrap $ accountId) > 0) `shouldBe` True :: Excepts '[RepositoryError] _ _
            & toThrow @RepositoryError
    describe "getByEmail should" $ do
      let sampleEmail = Email "abc@abc.de"
      it "return Nothing when repo is empty" $
        embed $
          do
            res <- ARC.getByEmail sampleEmail
            res `shouldBe` Nothing
            & catchAllToFail
      it "return Just when account with same Email exists" $
        embed $
          S.do
            accountNew <- S.coerce $ sampleIO & fmap (\x -> x{email = sampleEmail})
            accountId <- ARC.add accountNew
            res <- ARC.getByEmail sampleEmail
            let expAccount = A.fromAccountNew accountNew accountId
            S.coerce $ res `shouldBe` Just expAccount
            & catchAllToFail
      it "return Nothing when other accounts with different Emails exist" $
        embed $
          S.do
            accountNew <- S.coerce sampleIO
            ARC.add accountNew
            res <- ARC.getByEmail sampleEmail
            S.coerce $ res `shouldBe` Nothing
            & catchAllToFail
