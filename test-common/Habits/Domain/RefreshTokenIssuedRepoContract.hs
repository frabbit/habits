{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.Domain.RefreshTokenIssuedRepoContract where

import Control.Lens ((^.))
import GHC.Stack (HasCallStack)
import Habits.Domain.AccountId (AccountId)
import qualified Habits.Domain.RefreshTokenIssued as RTI
import Habits.Domain.RefreshTokenIssuedNew (RefreshTokenIssuedNew (_accountId))
import Habits.Domain.RefreshTokenIssuedRepo.Class
  ( RefreshTokenIssuedRepo,
  )
import qualified Habits.Domain.RefreshTokenIssuedRepo.Class as RTIC
import Haskus.Utils.Variant.Excepts (Excepts, evalE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Test.Hspec
  ( Spec,
    describe,
    it,
    parallel,
  )
import Test.Hspec.Expectations.Lifted
  ( shouldBe,
  )
import UnliftIO (MonadIO)
import Utils
  ( catchAllToFail,
    sampleIO,
  )
import Prelude hiding (id)

insertToken :: forall m. (RefreshTokenIssuedRepo m, _) => Excepts _ m _
insertToken = S.do
  new <- S.coerce sampleIO
  id <- RTIC.add new
  Just acc <- RTIC.getById id
  S.pure (acc, new, id)

insertTokenForAccountId :: forall m. (RefreshTokenIssuedRepo m, _) => AccountId -> Excepts _ m _
insertTokenForAccountId accId = S.do
  new <- S.coerce $ fmap (\x -> x {_accountId = accId}) sampleIO
  id <- RTIC.add new
  Just acc <- RTIC.getById id
  S.pure (acc, new, id)

mkSpec :: forall m. (MonadFail m, HasCallStack, MonadIO m, RefreshTokenIssuedRepo m) => (m () -> IO ()) -> Spec
mkSpec unlift = parallel $
  describe "RefreshTokenIssuedRepoContract" $ do

    let
      embed :: _ => _
      embed = unlift . evalE .  catchAllToFail
    describe "add should" $ do
      it "persist the account to the repository" $
        embed $
          S.do
            new <- S.coerce sampleIO
            id <- RTIC.add new
            acc <- RTIC.getById id
            S.coerce $ acc `shouldBe` Just (RTI.fromRefreshTokenIssuedNew new id)
    describe "getById should" $ do
      it "fail with AccountNotFound when repository is empty" . embed $
        S.do
          id <- S.coerce sampleIO
          res <- RTIC.getById id
          S.coerce $ res `shouldBe` Nothing
    describe "getByAccountId should" $ do
      it "return an empty array when repository is empty" . embed $
        S.do
          id <- S.coerce sampleIO
          res <- RTIC.getByAccountId id
          S.coerce $ res `shouldBe` []
      it "return an empty array when repository contains only elements for other accounts" . embed $
        S.do
          (new, id) <- S.coerce sampleIO
          RTIC.add new

          res <- RTIC.getByAccountId id
          S.coerce $ res `shouldBe` []
      it "return an array when repository contains tokens for account" . embed $
        S.do
          (acc, new, id) <- insertToken
          res <- RTIC.getByAccountId (acc ^. RTI.accountId)
          S.coerce $ res `shouldBe` [RTI.fromRefreshTokenIssuedNew new id]
      it "return an array when repository contains multiple tokens for account" . embed $
        S.do
          accountId <- S.coerce sampleIO
          (_, new1, id1) <- insertTokenForAccountId accountId
          (_, new2, id2) <- insertTokenForAccountId accountId
          res <- RTIC.getByAccountId accountId
          S.coerce $ res `shouldBe` [RTI.fromRefreshTokenIssuedNew new1 id1, RTI.fromRefreshTokenIssuedNew new2 id2]
    describe "deleteByAccountId should" $ do
      it "do nothing when repo is empty" . embed $
        S.do
          accountId <- S.coerce sampleIO
          res <- RTIC.deleteByAccountId accountId
          S.coerce $ res `shouldBe` ()
      it "not delete entities referencing other accounts" . embed $
        S.do
          (acc, _, id) <- insertToken
          accountId <- S.coerce sampleIO
          RTIC.deleteByAccountId accountId
          token <- RTIC.getById id
          S.coerce $ token `shouldBe` Just acc
      it "do delete entity when accountId matches" . embed $
        S.do
          (acc, _, _) <- insertToken
          let accountId = acc ^. RTI.accountId
          res <- RTIC.deleteByAccountId accountId
          accounts <- RTIC.getByAccountId accountId
          S.coerce $ res `shouldBe` ()
          S.coerce $ accounts `shouldBe` []
      it "delete multiple entities when accountId matches" . embed $
        S.do
          accountId <- S.coerce sampleIO
          insertTokenForAccountId accountId
          insertTokenForAccountId accountId
          RTIC.deleteByAccountId accountId
          accounts <- RTIC.getByAccountId accountId
          S.coerce $ accounts `shouldBe` []
    describe "deleteById should" $ do
      it "do nothing when repo is empty" . embed $
        S.do
          id <- S.coerce sampleIO
          res <- RTIC.deleteById id
          S.coerce $ res `shouldBe` ()
      it "not delete entities referencing other accounts" . embed $
        S.do
          (acc, _, id) <- insertToken
          rid <- S.coerce sampleIO
          RTIC.deleteById rid
          token <- RTIC.getById id
          S.coerce $ token `shouldBe` Just acc
      it "do delete entity when id matches" . embed $
        S.do
          (_, _, id) <- insertToken

          res <- RTIC.deleteById id
          accounts <- RTIC.getById id
          S.coerce $ res `shouldBe` ()
          S.coerce $ accounts `shouldBe` Nothing

