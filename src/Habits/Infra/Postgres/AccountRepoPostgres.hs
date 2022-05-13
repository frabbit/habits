{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Habits.Infra.Postgres.AccountRepoPostgres where

import Control.Lens (view)
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import qualified Data.Pool as P'
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Data.Variant
  ( throwM,
  )
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
        _getById
      ),
    Add,
    GetById,
  )
import Habits.Domain.Email (Email (..))
import Habits.Domain.Password (Password (..))
import qualified Habits.Infra.Postgres.Schema as S
import Haskus.Utils.Variant.Excepts (throwE)

accountIdToDomain :: P.Key S.Account -> AccountId
accountIdToDomain key = AccountId $ S.unAccountKey key

convertToDomain :: P.Entity S.Account -> Account
convertToDomain (P.Entity key a) =
  A.Account
    { A._name = S.accountName a,
      A._email = Email $ S.accountEmail a,
      A._accountId = accountIdToDomain key,
      A._password = Password $ S.accountPassword a
    }

withPool :: (MonadIO m) => P'.Pool P.SqlBackend -> _ -> m _
withPool pool = liftIO . flip P.runSqlPersistMPool pool

mkAdd :: forall m n. (Monad n, MonadIO m) => P'.Pool P.SqlBackend -> n (Add m)
mkAdd pool = pure f
  where
    f :: Add m
    f an = do
      uuid <- liftIO nextRandom

      let accountKey = S.AccountKey $ UUID.toText uuid
      withPool pool $ do
        P.insertKey accountKey $
          S.Account
            { S.accountName = view AN.name an,
              S.accountEmail = unEmail $ view AN.email an,
              S.accountPassword = unPassword $ view AN.password an
            }
      pure $ accountIdToDomain accountKey

mkGetById :: forall m n. (Monad n, MonadIO m) => P'.Pool P.SqlBackend -> n (GetById m)
mkGetById pool = pure f
  where
    f :: GetById m
    f (AccountId id') = do
      accountMaybe <- withPool pool $ do
        P.getEntity (S.AccountKey id')
      case accountMaybe of
        Nothing -> throwE AccountNotFoundError
        Just account -> pure $ convertToDomain account

mkAccountRepoPostgres ::
  (Monad n, MonadIO m) => P'.Pool P.SqlBackend -> n (AccountRepo m)
mkAccountRepoPostgres pool = do
  _add <- mkAdd pool
  _getById <- mkGetById pool
  pure $ AccountRepo {..}
