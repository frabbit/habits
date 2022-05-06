{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}
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
import           Control.Newtype                ( Newtype )
import           Data.Coerce                    ( coerce )
import           Data.Typeable                  ( Typeable )
import           Data.Variant                   ( CouldBe
                                                , Variant
                                                )
import           Habits.Domain.Account          ( Account )
import           Habits.Domain.Email            ( Email(Email) )
import           Habits.Domain.Password         ( Password(..) )
import           Veins.Data.HList               ( HList )
import qualified Veins.Data.HList              as HL
import           Veins.Data.Type.List           ( Contains )
import qualified Veins.Data.HList as HL

data AddError = AddError
  deriving (Show, Typeable)

instance Exception AddError

data AccountNotFoundError = AccountNotFoundError
  deriving (Show, Typeable, Eq, Ord)

instance Exception AccountNotFoundError

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
   . e `CouldBe` RepositoryError
  => e `CouldBe` AccountNotFoundError
  => AccountId
  -> ExceptT (Variant e) m Account

newtype AddW m = AddW { unAddW :: Add m }

type role AddW representational


newtype GetByIdW m = GetByIdW { unGetByIdW :: GetById m }

data AccountRepo m = AccountRepo
  { _add     :: AddW m
  , _getById :: GetByIdW m
  }

add :: forall m . Lens' (AccountRepo m) (AddW m)
add = lens get set
 where
  set :: AccountRepo m -> AddW m -> AccountRepo m
  set ar a = ar { _add = a }
  get :: AccountRepo m -> AddW m
  get AccountRepo { _add = a } = a

getById :: forall m . Lens' (AccountRepo m) (GetByIdW m)
getById = lens get set
 where
  set :: AccountRepo m -> GetByIdW m -> AccountRepo m
  set ar a = ar { _getById = a }
  get :: AccountRepo m -> GetByIdW m
  get AccountRepo { _getById = a } = a


mkStub :: (Monad m) => AccountRepo m
mkStub = AccountRepo
  { _add     = AddW $ \_ -> pure (AccountId "abc")
  , _getById = GetByIdW $ \_ -> pure
                 (A.Account { A._name      = "abc"
                            , A._email     = Email "abc@abd.de"
                            , A._accountId = AccountId "123"
                            , A._password  = Password "pw"
                            }
                 )
  }


