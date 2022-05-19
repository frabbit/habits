{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Habits.Infra.Memory.AccountRepoMemory where

import Control.Lens ((^.))
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Control.Monad.Reader (ReaderT)
import Data.Foldable (find)
import Data.Function ((&))
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Conc
  ( newTVarIO,
    readTVarIO,
  )
import Habits.Domain.Account (Account)
import qualified Habits.Domain.Account as A
import Habits.Domain.AccountId (AccountId (AccountId))
import qualified Habits.Domain.AccountRepo as AR
import Habits.Domain.AccountNotFoundError
    ( AccountNotFoundError(AccountNotFoundError) )
import Habits.Domain.AccountRepo
  ( AccountRepo
      ( AccountRepo,
        _add,
        _getById
      ),
    Add,
    GetById, GetByEmail,
  )
import Haskus.Utils.Variant.Excepts (throwE)
import UnliftIO
  ( TVar,
    atomically,
  )
import UnliftIO.STM (modifyTVar)
import qualified Veins.Data.ComposableEnv as CE

mkAdd ::
  forall n m. (Applicative n, MonadIO m) => TVar [Account] -> n (Add m)
mkAdd accountsVar = pure f
  where
    f :: Add m
    f an = do
      uuid <- liftIO nextRandom
      let id' = AccountId (toText uuid)
      let newAccount = A.fromAccountNew an id'
      atomically $ modifyTVar accountsVar $ \a -> reverse (newAccount : reverse a)
      pure id'

mkGetById ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [Account] ->
  n (GetById m)
mkGetById accountsVar = pure f
  where
    f :: GetById m
    f accountId = do
      accounts :: [Account] <- liftIO $ readTVarIO accountsVar
      let accountMaybe = find (\a -> (a ^. A.accountId) == accountId) accounts
      case accountMaybe of
        Nothing -> throwE AccountNotFoundError
        Just account -> pure account

mkGetByEmail ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [Account] ->
  n (GetByEmail m)
mkGetByEmail accountsVar = pure f
  where
    f :: GetByEmail m
    f email = do
      accounts :: [Account] <- liftIO $ readTVarIO accountsVar
      pure $ find (\a -> (a ^. A.email) == email) accounts


mkAccountRepoMemory :: (MonadIO n, MonadIO m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[AccountRepo m])
mkAccountRepoMemory = do
  accountsVar <- liftIO $ newTVarIO []
  _getById <- mkGetById accountsVar
  _getByEmail <- mkGetByEmail accountsVar
  _add <- mkAdd accountsVar
  pure $ CE.empty & CE.insert AccountRepo {_add, _getById, AR._getByEmail = _getByEmail }
