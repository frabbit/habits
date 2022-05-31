module Habits.Domain.Clock where

import Veins.Data.ToSymbol (ToSymbol)
import Control.Monad.Reader (MonadReader, asks, ReaderT)
import qualified Veins.Data.Has as Has
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad (join)
import qualified Veins.Data.ComposableEnv as CE
import Data.Function ((&))
import Control.Monad.IO.Class (MonadIO (liftIO))

type GetNow m = m UTCTime

data Clock m = Clock
  { _getNow :: GetNow m
  }

getNow :: forall m env. (Has.Has (Clock m) env, MonadReader env m) => GetNow m
getNow = join . asks $ _getNow . Has.get

mkGetNow :: forall m env n. ( Has.Has (Clock m) env, MonadReader env n) => n (GetNow m)
mkGetNow = asks (_getNow . Has.get)

type instance ToSymbol (Clock m) = "Clock"

mkStaticClock :: forall n m. (Monad n, Monad m) => UTCTime -> ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[Clock m])
mkStaticClock timeNow = pure $ CE.empty & CE.insert Clock {_getNow = pure timeNow }

mkLiveClock :: forall n m. (Monad n, MonadIO m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[Clock m])
mkLiveClock = pure $ CE.empty & CE.insert Clock {_getNow = liftIO getCurrentTime }