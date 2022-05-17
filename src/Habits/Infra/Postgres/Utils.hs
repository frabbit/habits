{-# LANGUAGE PartialTypeSignatures #-}

module Habits.Infra.Postgres.Utils where

import Control.Lens (view)
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import qualified Data.Pool as P'
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import qualified Database.Persist.Postgresql as P
import Habits.Domain.Account (Account)
import qualified Habits.Domain.Account as A
import Habits.Domain.AccountId (AccountId (AccountId))
import qualified Habits.Domain.AccountNew as AN
import Habits.Domain.AccountRepo
  ( AccountNotFoundError
      ( AccountNotFoundError
      ),
    AccountRepo
      ( AccountRepo,
        _add,
        _getById,
        _getByEmail
      ),
    Add,
    GetById, GetByEmail,
  )
import Habits.Domain.Email (Email (..))
import Habits.Domain.Password (Password (..))
import qualified Habits.Infra.Postgres.Schema as S
import Haskus.Utils.Variant.Excepts (throwE)
import Database.Persist ((==.), SelectOpt (LimitTo))
import qualified Veins.Data.ComposableEnv as CE
import Data.Function ((&))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import UnliftIO.Resource (ResourceT)

withPool :: (MonadIO m) => P'.Pool P.SqlBackend -> ReaderT P.SqlBackend (NoLoggingT (ResourceT IO)) a -> m a
withPool pool = liftIO . flip P.runSqlPersistMPool pool