{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.Domain.RefreshTokenIssuedRepoContract where

import Data.Function ((&))
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import Habits.Domain.Email (Email (Email))
import qualified Habits.Domain.RefreshTokenIssued as RTI
import Habits.Domain.RefreshTokenIssuedId (RefreshTokenIssuedId)
import qualified Habits.Domain.RefreshTokenIssuedId as RefreshTokenIssuedId
import Habits.Domain.RefreshTokenIssuedRepo.Class
  ( RefreshTokenIssuedRepo,
  )
import qualified Habits.Domain.RefreshTokenIssuedRepo.Class as RTIC
import Habits.Domain.RepositoryError (RepositoryError)
import Haskus.Utils.Variant.Excepts (Excepts, catchLiftLeft, evalE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
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
  ( catchAllToFail,
    sampleIO,
    toThrow,
  )
import Prelude hiding (id)

mkSpec :: forall m. (HasCallStack, MonadIO m, RefreshTokenIssuedRepo m) => (m () -> IO ()) -> Spec
mkSpec unlift = parallel $
  describe "RefreshTokenIssuedRepoContract" $ do
    let embed = unlift . evalE
    describe "add should" $ do
      it "persist the account to the repository" $
        embed $
          S.do
            new <- S.coerce sampleIO
            id <- RTIC.add new
            acc <- RTIC.getById id
            S.coerce $ acc `shouldBe` Just (RTI.fromRefreshTokenIssuedNew new id)
            & toThrow @RepositoryError
    describe "getById should" $ do
      it "fail with AccountNotFound when repository is empty" . embed $
          S.do
            id <- S.coerce sampleIO
            res <- RTIC.getById id
            S.coerce $ res `shouldBe` Nothing
            & toThrow @RepositoryError
