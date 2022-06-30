{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Habits.UseCases.CreateHabitSpec where

import qualified Veins.Data.ComposableEnv as CE
import qualified Haskus.Utils.Variant.Excepts.Syntax as S

import Habits.Test.Prelude
import Habits.UseCases.CreateHabit (CreateHabit)
import Habits.UseCases.CreateHabit.Live (mkCreateHabitLive)
import qualified Veins.Test.AppTH as AppTH
import Utils (catchAllToFail, sampleIO, expectError)
import Habits.UseCases.CreateHabit.Class (createHabit)
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import qualified Habits.Domain.AccountRepo.Class as A
import Habits.Domain.AccountRepo (AccountRepo)
import Habits.Infra.Memory.AccountRepoMemory (mkAccountRepoMemory)
import qualified Habits.Domain.HabitRepo.Class as H
import Habits.Domain.Habit (toHabitNew)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Habits.Domain.HabitRepo (HabitRepo)
import Habits.Infra.Memory.HabitRepoMemory (mkHabitRepoMemory)
import Habits.Domain.HabitNew (HabitNew)
import Habits.UseCases.CreateHabit.CreateHabitRequest (CreateHabitRequest (..))


type Env m = CE.MkSorted '[CreateHabit m, AccountRepo m, HabitRepo m]

envLayer :: forall m n. (MonadIO n, MonadIO m) => CE.ReaderCE '[] n (Env m)
envLayer = mkCreateHabitLive <<-&& mkAccountRepoMemory <<-&& mkHabitRepoMemory

AppTH.mkBoilerplate "runApp" ''Env

runWithEnv :: _ -> _ b -> IO b
runWithEnv layer app = do
  env <- runReaderT layer CE.empty
  runApp env app

embed :: _ => _
embed = runWithEnv (envLayer :: _) . evalE . catchAllToFail

habitNewToRegisterRequest :: HabitNew -> CreateHabitRequest
habitNewToRegisterRequest hn = CreateHabitRequest {name = hn.name, accountId = hn.accountId }

spec :: Spec
spec = describe "createHabit should" $ do
  it "fail with AccountNotFoundError when Account for AccountId does not exist." . embed $ S.do
    req <- S.coerce sampleIO
    createHabit req
    & expectError @AccountNotFoundError
  it "successfully store the habit in the habit repository." . embed $ S.do
    an <- S.coerce sampleIO
    accountId <- A.add an
    hn <- S.coerce sampleIO <&> \x -> x{accountId}
    let req = habitNewToRegisterRequest hn
    res <- createHabit req
    [entity] <- H.getByAccountId accountId
    S.coerce $ toHabitNew entity `shouldBe` hn

    S.coerce $ res.habitId `shouldBe` entity.habitId
