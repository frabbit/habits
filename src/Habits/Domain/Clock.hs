module Habits.Domain.Clock where

import Veins.Data.ToSymbol (ToSymbol)
import Control.Monad.Reader (MonadReader, asks)
import qualified Veins.Data.Has as Has
import Data.Time (UTCTime)
import Control.Monad (join)

type GetNow m = m UTCTime

data Clock m = Clock
  { _getNow :: GetNow m
  }

getNow :: forall m env. (Has.Has (Clock m) env, MonadReader env m) => GetNow m
getNow = join . asks $ _getNow . Has.get

mkGetNow :: forall m env n. ( Has.Has (Clock m) env, MonadReader env n) => n (GetNow m)
mkGetNow = asks (_getNow . Has.get)

type instance ToSymbol (Clock m) = "Clock"

