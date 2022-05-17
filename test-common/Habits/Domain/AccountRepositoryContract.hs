{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.Domain.AccountRepositoryContract where

import Data.Function ((&))
import qualified Data.Functor
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import qualified Habits.Domain.Account as A
import Habits.Domain.AccountId (AccountId)
import qualified Habits.Domain.AccountId as AccountId
import Habits.Domain.AccountRepo
  ( AccountNotFoundError
      ( AccountNotFoundError
      ),
    AddError,
    RepositoryError,
  )
import Habits.Domain.AccountRepo.Class
  ( AccountRepo,
  )
import qualified Habits.Domain.AccountRepo.Class as ARC
import Habits.TH.TypeOf (getStaticDecl)
import Test.Hspec
  ( Spec,
    describe,
    it,
    parallel,
  )
import Test.Hspec.Expectations.Lifted
  ( expectationFailure,
    shouldBe,
  )
import UnliftIO (MonadIO)
import Utils
  ( sampleIO,
    toThrow,
  )
import Haskus.Utils.Variant.Excepts (Excepts, catchLiftLeft, evalE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S

$(getStaticDecl 'Data.Functor.fmap)

mkSpec :: forall m . (HasCallStack, MonadIO m, AccountRepo m) => (m () -> IO ()) -> Spec
mkSpec unlift = parallel $
  describe "AccountRepositoryContract" $ do
    let embed = unlift . evalE
    describe "add should" $ do
      it "persist the account to the repository" $ embed $ S.do
        accountNew <- S.coerce sampleIO
        accountId <- ARC.add accountNew
        acc <- ARC.getById accountId
        S.coerce $ acc `shouldBe` A.fromAccountNew accountNew accountId
        S.coerce $ ((Text.length . AccountId.unwrap $ accountId) > 0) `shouldBe` True
        & toThrow @AddError
        & toThrow @RepositoryError
        & toThrow @AccountNotFoundError
    describe "getById should" $ do
      it "fail with AccountNotFound when repository is empty" $ embed $ do
        (accountId :: AccountId) <- sampleIO
        res <-
          (ARC.getById accountId & fmap Right)
          & catchLiftLeft (\(e :: AccountNotFoundError) -> pure . Left $ e)
        case res of
          Left e -> e `shouldBe` AccountNotFoundError
          Right _ -> expectationFailure "AccountNotFoundError expected"

        ((Text.length . AccountId.unwrap $ accountId) > 0) `shouldBe` True :: Excepts '[RepositoryError] _ _
        & toThrow @RepositoryError

