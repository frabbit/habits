module Habits.Domain.AccountRepo where

import qualified Habits.Domain.Account         as A
import           Habits.Domain.AccountId        ( AccountId(AccountId) )
import           Habits.Domain.AccountNew      as AccountNew
                                                ( AccountNew )

import           Control.Exception              ( Exception )
import           Control.Lens                   ( Lens'
                                                , lens
                                                )
import           Control.Monad.Trans.Except     ( ExceptT )
import           Data.Typeable                  ( Typeable )
import           Data.Variant                   ( CouldBe
                                                , Variant
                                                )
import           Habits.Domain.Account          ( Account )
import           Habits.Domain.Email            ( Email(Email) )

data AddError = AddError
  deriving (Show, Typeable)

instance Exception AddError

data RepositoryError = RepositoryError
  deriving (Show, Typeable)

instance Exception RepositoryError

type Add m
  =  forall e
   . (e `CouldBe` AddError)
  => AccountNew
  -> ExceptT (Variant e) m AccountId

type GetById m
  =  forall e
   . (e `CouldBe` RepositoryError)
  => AccountId
  -> ExceptT (Variant e) m Account

newtype WrapAdd m = WrapAdd { unWrapAdd :: Add m }

newtype WrapGetById m = WrapGetById { unWrapGetById :: GetById m }

data AccountRepo m = AccountRepo
  { _add     :: WrapAdd m
  , _getById :: WrapGetById m
  }

add :: forall m . Lens' (AccountRepo m) (WrapAdd m)
add = lens get set
 where
  set :: AccountRepo m -> WrapAdd m -> AccountRepo m
  set ar a = ar { _add = a }
  get :: AccountRepo m -> WrapAdd m
  get AccountRepo { _add = a } = a

getById :: forall m . Lens' (AccountRepo m) (WrapGetById m)
getById = lens get set
 where
  set :: AccountRepo m -> WrapGetById m -> AccountRepo m
  set ar a = ar { _getById = a }
  get :: AccountRepo m -> WrapGetById m
  get AccountRepo { _getById = a } = a


mkStub :: (Monad m) => AccountRepo m
mkStub = AccountRepo
  { _add     = WrapAdd $ \_ -> pure (AccountId "abc")
  , _getById = WrapGetById $ \_ -> pure
                 (A.Account { A._name      = "abc"
                            , A._email     = Email "abc@abd.de"
                            , A._accountId = AccountId "123"
                            }
                 )
  }
