{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Habits.Infra.Postgres.AccountRepoPostgres where

import qualified Data.Pool as P'
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist (SelectOpt (LimitTo), (==.), (=.))
import qualified Database.Persist.Postgresql as P
import Habits.Domain.Account (Account)
import qualified Habits.Domain.Account as A
import Habits.Domain.AccountId (AccountId (AccountId))
import Habits.Domain.AccountNotFoundError (AccountNotFoundError (AccountNotFoundError))
import Habits.Domain.AccountRepo
  ( AccountRepo (..),
    Add,
    GetByEmail,
    GetById,
    Update,
  )
import Habits.Domain.Email (Email (..))
import Habits.Domain.PasswordHash (PasswordHash (PasswordHash, unPasswordHash))
import qualified Habits.Infra.Postgres.Schema as S
import Habits.Infra.Postgres.Utils (withPool)
import Habits.Prelude
import qualified Veins.Data.ComposableEnv as CE

accountIdToDomain :: P.Key S.Account -> AccountId
accountIdToDomain key = AccountId $ S.unAccountKey key

convertToDomain :: P.Entity S.Account -> Account
convertToDomain (P.Entity key a) =
  A.Account
    { name = S.accountName a,
      email = Email $ S.accountEmail a,
      emailConfirmed = S.accountEmailConfirmed a,
      accountId = accountIdToDomain key,
      password = PasswordHash $ S.accountPassword a
    }

{- HLINT ignore mkAdd "Redundant bracket" -}
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
            { S.accountName = an.name,
              S.accountEmail = unEmail an.email,
              S.accountEmailConfirmed = an.emailConfirmed,
              S.accountPassword = unPasswordHash an.password
            }
      pure $ accountIdToDomain accountKey

{- HLINT ignore mkAdd "Redundant bracket" -}
mkUpdate :: forall m n. (Monad n, MonadIO m) => P'.Pool P.SqlBackend -> n (Update m)
mkUpdate pool = pure f
  where
    f :: Update m
    f au id = do
      let accountKey = S.AccountKey id.unAccountId
      withPool pool $ do
        let
          updates =
            maybe [] (\x -> [S.AccountEmailConfirmed =. x]) au.emailConfirmed
            <> maybe [] (\x -> [S.AccountEmail =. x.unEmail]) au.email
        P.update accountKey updates

      pure ()

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

mkGetByEmail :: forall m n. (Monad n, MonadIO m) => P'.Pool P.SqlBackend -> n (GetByEmail m)
mkGetByEmail pool = pure f
  where
    f :: GetByEmail m
    f (Email e) = do
      accountMaybe <- withPool pool $ do
        P.selectFirst [S.AccountEmail ==. e] [LimitTo 1]
      let acc = convertToDomain <$> accountMaybe
      pure acc

mkAccountRepoPostgres ::
  forall n m.
  (Monad n, MonadIO m) =>
  P'.Pool P.SqlBackend ->
  CE.LayerCE '[] n '[AccountRepo m]
mkAccountRepoPostgres pool = do
  add <- mkAdd pool
  getById <- mkGetById pool
  getByEmail <- mkGetByEmail pool
  update <- mkUpdate pool
  pure $ CE.singleton (AccountRepo {..})
