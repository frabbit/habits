module Habits.Domain.TimeProvider where

import Veins.Data.ToSymbol (ToSymbol)
import Control.Monad.Reader (MonadReader, asks)
import qualified Veins.Data.Has as Has
import Data.Time (UTCTime)
import Control.Monad (join)

type GetNow m = m UTCTime

data TimeProvider m = TimeProvider
  { _getNow :: GetNow m
  }

getNow :: forall m env. (Has.Has (TimeProvider m) env, MonadReader env m) => GetNow m
getNow = join . asks $ _getNow . Has.get

mkGetNow :: forall m env n. ( Has.Has (TimeProvider m) env, MonadReader env n) => n (GetNow m)
mkGetNow = asks (_getNow . Has.get)

type instance ToSymbol (TimeProvider m) = "TimeProvider"

