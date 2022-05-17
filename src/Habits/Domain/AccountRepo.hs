{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}

module Habits.Domain.AccountRepo where

import Control.Exception (Exception)
import Control.Lens
  ( Lens',
    lens,
  )
import qualified Control.Lens as L
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader.Class (asks)
import Data.Typeable (Typeable)
import Habits.Domain.Account (Account)
import qualified Habits.Domain.Account as A
import Habits.Domain.AccountId (AccountId (AccountId))
import Habits.Domain.AccountNew as AccountNew
  ( AccountNew,
  )
import Habits.Domain.Email (Email (Email))
import Habits.Domain.Password (Password (..))
import Haskus.Utils.Variant.Excepts (Excepts)
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (ToSymbol)

data AddError = AddError
  deriving (Show, Typeable)

instance Exception AddError

data AccountNotFoundError = AccountNotFoundError
  deriving (Show, Typeable, Eq, Ord)

instance Exception AccountNotFoundError

data RepositoryError = RepositoryError
  deriving (Show, Typeable)

instance Exception RepositoryError

type Add m =
  AccountNew ->
  Excepts '[AddError] m AccountId

type GetByEmail m =
  Email ->
  Excepts '[RepositoryError] m (Maybe Account)

type GetById m =
  AccountId ->
  Excepts '[RepositoryError, AccountNotFoundError] m Account

data AccountRepo m = AccountRepo
  { _add :: Add m,
    _getById :: GetById m,
    _getByEmail :: GetByEmail m
  }

type instance ToSymbol (AccountRepo m) = "AccountRepo"

setAdd :: Add m -> AccountRepo m -> AccountRepo m
setAdd = L.set addL

setGetById :: GetById m -> AccountRepo m -> AccountRepo m
setGetById = L.set getByIdL

addL :: forall m. Lens' (AccountRepo m) (Add m)
addL = lens get set
  where
    set :: AccountRepo m -> Add m -> AccountRepo m
    set ar a = ar {_add = a}
    get :: AccountRepo m -> Add m
    get AccountRepo {_add = a} = a

getByIdL :: forall m. Lens' (AccountRepo m) (GetById m)
getByIdL = lens get set
  where
    set :: AccountRepo m -> GetById m -> AccountRepo m
    set ar a = ar {_getById = a}
    get :: AccountRepo m -> GetById m
    get AccountRepo {_getById = a} = a

type AccountRepoR env = AccountRepo (ReaderT env IO)

add :: forall m env. (Has.Has (AccountRepo m) env, MonadReader env m) => Add m
add r = do
  AccountRepo {_add = f} <- asks (Has.get @(AccountRepo m))
  f r

getById :: forall m env. (Has.Has (AccountRepo m) env, MonadReader env m) => GetById m
getById r = do
  AccountRepo {_getById = f} <- asks (Has.get @(AccountRepo m))
  f r

getByEmail :: forall m env. (Has.Has (AccountRepo m) env, MonadReader env m) => GetByEmail m
getByEmail r = do
  AccountRepo {_getByEmail = f} <- asks (Has.get @(AccountRepo m))
  f r

mkStub :: (Monad m) => AccountRepo m
mkStub =
  AccountRepo
    { _add = \_ -> pure (AccountId "abc"),
      _getByEmail = \_ ->
        pure . Just $
          ( A.Account
              { A._name = "abc",
                A._email = Email "abc@abd.de",
                A._accountId = AccountId "123",
                A._password = Password "pw"
              }
          ),
      _getById = \_ ->
        pure
          ( A.Account
              { A._name = "abc",
                A._email = Email "abc@abd.de",
                A._accountId = AccountId "123",
                A._password = Password "pw"
              }
          )
    }
