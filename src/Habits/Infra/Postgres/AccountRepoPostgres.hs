{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
module Habits.Infra.Postgres.AccountRepoPostgres where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Variant                   ( throwM
                                                )
import           Habits.Domain.Account          ( Account )
import qualified Habits.Domain.Account         as A
import           Habits.Domain.AccountId        ( AccountId(AccountId) )

import           Control.Lens                   ( view )
import qualified Data.Pool                     as P'
import           Data.UUID.V4                   ( nextRandom )
import qualified Database.Persist.Postgresql   as P
import qualified Habits.Domain.AccountNew      as AN
import           Habits.Domain.AccountRepo      ( AccountNotFoundError
                                                  ( AccountNotFoundError
                                                  )
                                                , AccountRepo
                                                  ( AccountRepo
                                                  , _add
                                                  , _getById
                                                  )
                                                , AddW(..)
                                                , GetByIdW(..)
                                                )
import           Habits.Domain.Email            ( Email(..) )
import qualified Habits.Infra.Postgres.Schema  as S

import qualified Data.UUID                     as UUID
import           Habits.Domain.Password         ( Password(..) )

accountIdToDomain :: P.Key S.Account -> AccountId
accountIdToDomain key = AccountId $ S.unAccountKey key

convertToDomain :: P.Entity S.Account -> Account
convertToDomain (P.Entity key a) = A.Account
  { A._name      = S.accountName a
  , A._email     = Email "abs@de.de"
  , A._accountId = accountIdToDomain key
  , A._password  = Password "abc"
  }

withPool :: (MonadIO m) => P'.Pool P.SqlBackend -> _ -> m _
withPool pool = liftIO . flip P.runSqlPersistMPool pool

mkAdd :: forall m n . (Monad n, MonadIO m) => P'.Pool P.SqlBackend -> n (AddW m)
mkAdd pool = pure $ AddW f
 where
  f an = do
    uuid <- liftIO nextRandom

    let accountKey = S.AccountKey $ UUID.toText uuid
    withPool pool $ do
      P.insertKey accountKey $ S.Account
        { S.accountName  = view AN.name an
        , S.accountEmail = unEmail $ view AN.email an
        }
    pure $ accountIdToDomain accountKey



mkGetById :: forall m n . (Monad n, MonadIO m) => P'.Pool P.SqlBackend -> n (GetByIdW m)
mkGetById pool = pure $ GetByIdW f
 where
  f (AccountId id') = do
    accountMaybe <- withPool pool $ do
      P.getEntity (S.AccountKey id')
    case accountMaybe of
      Nothing      -> throwM AccountNotFoundError
      Just account -> pure $ convertToDomain account

mkAccountRepoPostgres
  :: (Monad n, MonadIO m) => P'.Pool P.SqlBackend -> n (AccountRepo m)
mkAccountRepoPostgres pool = do
  _add <- mkAdd pool
  _getById <- mkGetById pool
  pure $ AccountRepo { .. }
