{-# LANGUAGE PartialTypeSignatures #-}

module Habits.Infra.Postgres.Utils where

import Prelude

import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import qualified Data.Pool as P'
import qualified Database.Persist.Postgresql as P
import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import UnliftIO.Resource (ResourceT)

withPool :: (MonadIO m) => P'.Pool P.SqlBackend -> ReaderT P.SqlBackend (NoLoggingT (ResourceT IO)) a -> m a
withPool pool = liftIO . flip P.runSqlPersistMPool pool