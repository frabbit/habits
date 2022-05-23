{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Habits.Infra.Memory.RefreshTokenIssuedRepoMemory where

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
import qualified Habits.Domain.RefreshTokenIssued as RTI
import Habits.Domain.AccountId (AccountId (AccountId))
import qualified Habits.Domain.AccountRepo as AR
import Habits.Domain.AccountNotFoundError
    ( AccountNotFoundError(AccountNotFoundError) )
import Haskus.Utils.Variant.Excepts (throwE)
import UnliftIO
  ( TVar,
    atomically,
  )
import UnliftIO.STM (modifyTVar)
import qualified Veins.Data.ComposableEnv as CE
import Habits.Domain.RefreshTokenIssuedRepo (RefreshTokenIssuedRepo (RefreshTokenIssuedRepo, _getById, _add), Add, GetById)
import Habits.Domain.RefreshTokenIssued (RefreshTokenIssued)
import qualified Habits.Domain.RefreshTokenIssued as A
import Habits.Domain.RefreshTokenIssuedId (RefreshTokenIssuedId(..))

mkAdd ::
  forall n m. (Applicative n, MonadIO m) => TVar [RefreshTokenIssued] -> n (Add m)
mkAdd accountsVar = pure f
  where
    f :: Add m
    f an = do
      uuid <- liftIO nextRandom
      let id' = RefreshTokenIssuedId (toText uuid)
      let newAccount = RTI.fromRefreshTokenIssuedNew an id'
      atomically $ modifyTVar accountsVar $ \a -> reverse (newAccount : reverse a)
      pure id'

mkGetById ::
  forall m n.
  (Applicative n, MonadIO m) =>
  TVar [RefreshTokenIssued] ->
  n (GetById m)
mkGetById accountsVar = pure f
  where
    f :: GetById m
    f accountId = do
      accounts :: [RefreshTokenIssued] <- liftIO $ readTVarIO accountsVar
      pure $ find (\a -> (a ^. A.refreshTokenIssuedId) == accountId) accounts



mkRefreshTokenIssuedRepoMemory :: (MonadIO n, MonadIO m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[RefreshTokenIssuedRepo m])
mkRefreshTokenIssuedRepoMemory = do
  entitiesVar <- liftIO $ newTVarIO []
  _getById <- mkGetById entitiesVar
  _add <- mkAdd entitiesVar
  pure $ CE.empty & CE.insert RefreshTokenIssuedRepo {_add, _getById }
