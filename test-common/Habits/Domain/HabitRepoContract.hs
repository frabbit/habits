{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Habits.Domain.HabitRepoContract where

import GHC.Stack (HasCallStack)
import qualified Habits.Domain.Habit as H
import Habits.Domain.HabitRepo.Class
  ( HabitRepoM,
  )
import qualified Habits.Domain.HabitRepo.Class as C
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
import Veins.Test.QuickCheck (propertyRuns)

mkSpec :: forall m. (HasCallStack, MonadIO m, HabitRepoM m) => (m () -> IO ()) -> Spec
mkSpec unlift = parallel $
  describe "HabitRepoContract" $ do
    let embed :: _ => _
        embed = unlift . evalE . catchAllToFail
    describe "add should" $ do
      it "persist the habit to the repository" . propertyRuns 4 $ \habitNew -> embed $ S.do
        habitId <- C.add habitNew
        habit <- C.getById habitId
        S.coerce $ habit `shouldBe` Just (H.fromHabitNew habitNew habitId)
    describe "getById should" $ do
      it "return Nothing when repository is empty" . embed $ do
        habitId <- sampleIO
        r <- C.getById habitId
        r `shouldBe` Nothing

