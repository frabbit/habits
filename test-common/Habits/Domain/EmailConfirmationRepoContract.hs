{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Habits.Domain.EmailConfirmationRepoContract where

import GHC.Stack (HasCallStack)
import Habits.Domain.EmailConfirmation (fromEmailConfirmationNew)
import Habits.Domain.EmailConfirmationRepo.Class (EmailConfirmationRepoM (add, getByNonce), getById)
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
  )

mkSpec :: forall m. (HasCallStack, MonadIO m, EmailConfirmationRepoM m) => (m () -> IO ()) -> Spec
mkSpec unlift = parallel $
  fdescribe "EmailConfirmationRepoContract" $ do
    let embed = unlift . evalE . catchAllToFail
    describe "add should" $ do
      it "persist the EmailConfirmation to the repository" . embed $
        S.do
          new <- S.coerce sampleIO
          entityId <- add new
          acc <- getById entityId
          S.coerce $ acc `shouldBe` Just (fromEmailConfirmationNew new entityId)
    describe "getById should" $ do
      it "return Nothing when repository is empty" $
        embed $
          do
            id <- sampleIO
            res <- getById id
            res `shouldBe` Nothing
    describe "getByNonce should" $ do
      it "return Nothing when repository is empty" $
        embed $
          do
            id <- sampleIO
            res <- getByNonce id
            res `shouldBe` Nothing
      it "return first entity containing that nonce" . embed $
        S.do
          new <- S.coerce sampleIO
          id <- add new
          res <- getByNonce new.emailConfirmationNonce
          S.coerce $ res `shouldBe` Just (fromEmailConfirmationNew new id)
