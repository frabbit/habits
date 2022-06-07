{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Habits.Domain.Clock where

import Habits.Prelude

import Veins.Data.ToSymbol (ToSymbol)
import qualified Veins.Data.Has as Has
import Data.Time (UTCTime, getCurrentTime)
import qualified Veins.Data.ComposableEnv as CE

type GetNow m = m UTCTime

data Clock m = Clock
  { _getNow :: GetNow m
  }

getClock :: (MonadReader r n, Has.Has (Clock m) r) => n (Clock m)
getClock = asks Has.get

getNow :: forall m. Clock m -> GetNow m
getNow = (._getNow)

mkGetNow :: forall m env n. ( Has.Has (Clock m) env, MonadReader env n) => n (GetNow m)
mkGetNow = asks (_getNow . Has.get)

type instance ToSymbol (Clock m) = "Clock"

mkClockStatic :: forall n m. (Monad n, Monad m) => UTCTime -> ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[Clock m])
mkClockStatic timeNow = pure $ CE.singleton Clock {_getNow = pure timeNow }

mkClockLive :: forall n m. (Monad n, MonadIO m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[Clock m])
mkClockLive = pure $ CE.singleton Clock {_getNow = liftIO getCurrentTime }