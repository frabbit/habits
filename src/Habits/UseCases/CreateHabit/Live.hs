{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Habits.UseCases.CreateHabit.Live where


import Habits.Prelude
import qualified Veins.Data.ComposableEnv as CE
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Habits.UseCases.CreateHabit (CreateHabitExec, CreateHabit (CreateHabit))
import Habits.Domain.AccountNotFoundError (AccountNotFoundError(AccountNotFoundError))
import Habits.Domain.AccountRepo (AccountRepo(..), getAccountRepo)
import Habits.Domain.HabitRepo (HabitRepo, getHabitRepo)
import Habits.UseCases.CreateHabit.CreateHabitResponse (CreateHabitResponse(CreateHabitResponse), habitId)
import Habits.Domain.HabitNew (HabitNew(..))

type Deps m = '[AccountRepo m, HabitRepo m]

mkCreateHabit :: forall n m. ( Monad m, Monad n) =>ReaderT (CE.MkSorted (Deps m)) n (CreateHabitExec m)
mkCreateHabit = do
  ar <- getAccountRepo
  hr <- getHabitRepo
  pure $ \req -> liftE $ S.do
    _ <- ar.getById req.accountId
    habitId <- hr.add $ HabitNew { accountId = req.accountId, name = req.name }
    S.pure $ CreateHabitResponse { habitId }

mkCreateHabitLive :: forall n m. (Monad n, Monad m) => ReaderT (CE.ComposableEnv (Deps m)) n (CE.ComposableEnv '[CreateHabit m])
mkCreateHabitLive = CE.singleton . CreateHabit <$> mkCreateHabit

