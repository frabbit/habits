{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Habits.Domain.EmailConfirmationRepoContract where

import Habits.Test.Prelude

import GHC.Stack (HasCallStack)

import Test.Hspec
  ( parallel,
  )
import Test.Hspec.Expectations.Lifted
  (
    shouldBe,
  )
import Utils
  ( catchAllToFail,
    sampleIO,
  )
import Habits.Domain.EmailConfirmationRepo.Class (EmailConfirmationRepoM (add), getById)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Habits.Domain.EmailConfirmation (fromEmailConfirmationNew)

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
