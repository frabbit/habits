{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Habits.Infra.Memory.AccountRepoMemory where
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Habits.Domain.Account          ( Account )
import qualified Habits.Domain.Account         as A
import           Habits.Domain.AccountId        ( AccountId(AccountId) )
import           UnliftIO                       ( TVar
                                                , atomically


                                                )

import           Control.Lens                   ( (^.) )
import           Data.Foldable                  ( find )
import           Data.UUID                      ( toText )
import           Data.UUID.V4                   ( nextRandom )
import           GHC.Conc                       ( newTVarIO
                                                , readTVarIO
                                                )
import           Habits.Domain.AccountRepo      ( AccountNotFoundError
                                                  ( AccountNotFoundError
                                                  )
                                                , AccountRepo
                                                  ( AccountRepo
                                                  , _add
                                                  , _getById
                                                  )

                                                , Add, GetById
                                                )
import           UnliftIO.STM                   ( modifyTVar )
import Haskus.Utils.Variant.Excepts (throwE)
import qualified Veins.Data.ComposableEnv as CE
import Data.Function ((&))
import Control.Monad.Reader (ReaderT)

mkAdd
  :: forall n m . (Applicative n, MonadIO m) => TVar [Account] -> n (Add m)
mkAdd accountsVar = pure f
 where
  f :: Add m
  f an = do
    uuid <- liftIO nextRandom
    let id'        = AccountId (toText uuid)
    let newAccount = A.fromAccountNew an id'
    atomically $ modifyTVar accountsVar $ \a -> reverse (newAccount : reverse a)
    pure id'

mkGetById
  :: forall m n
   . (Applicative n, MonadIO m)
  => TVar [Account]
  -> n (GetById m)
mkGetById accountsVar = pure f
 where
  f :: GetById m
  f accountId = do
    accounts :: [Account] <- liftIO $ readTVarIO accountsVar
    let accountMaybe = find (\a -> (a ^. A.accountId) == accountId) accounts
    case accountMaybe of
      Nothing      -> throwE AccountNotFoundError
      Just account -> pure account

mkAccountRepoMemory :: (MonadIO n, MonadIO m) => ReaderT (CE.ComposableEnv '[]) n (CE.ComposableEnv '[AccountRepo m])
mkAccountRepoMemory = do
  accountsVar <- liftIO $ newTVarIO []
  _getById    <- mkGetById accountsVar
  _add        <- mkAdd accountsVar
  pure $ CE.empty & CE.insert AccountRepo { _add, _getById }


