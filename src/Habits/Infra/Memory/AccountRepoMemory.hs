{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Habits.Infra.Memory.AccountRepoMemory where

import Control.Lens ((^.))
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Foldable (find)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Data.Variant
  ( throwM,
  )
import GHC.Conc
  ( newTVarIO,
    readTVarIO,
  )
import Habits.Domain.Account (Account)
import qualified Habits.Domain.Account as A
import Habits.Domain.AccountId (AccountId (AccountId))
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
    AddW (AddW),
    GetById,
    GetByIdW (GetByIdW),
  )
import UnliftIO
  ( TVar,
    atomically,
  )
import UnliftIO.STM (modifyTVar)

mkAdd ::
  forall n m. (Applicative n, MonadIO m) => TVar [Account] -> n (AddW m)
mkAdd accountsVar = pure $ AddW f
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
  n (GetByIdW m)
mkGetById accountsVar = pure $ GetByIdW f
  where
    f :: GetById m
    f accountId = do
      accounts :: [Account] <- liftIO $ readTVarIO accountsVar
      let accountMaybe = find (\a -> (a ^. A.accountId) == accountId) accounts
      case accountMaybe of
        Nothing -> throwM AccountNotFoundError
        Just account -> pure account

mkAccountRepoMemory :: (MonadIO n, MonadIO m) => n (AccountRepo m)
mkAccountRepoMemory = do
  accountsVar <- liftIO $ newTVarIO []
  _getById <- mkGetById accountsVar
  _add <- mkAdd accountsVar
  pure $ AccountRepo {_add, _getById}
