module Habits.Domain.AccountRepo where

import           Habits.Domain.AccountId        ( AccountId )
import           Habits.Domain.AccountNew      as AccountNew
                                                ( AccountNew )

import           Control.Exception              ( Exception )
import           Control.Monad.Trans.Except     ( ExceptT )
import           Data.Typeable                  ( Typeable )
import           Data.Variant                   ( CouldBe
                                                , Variant
                                                )

data AddError = AddError
  deriving (Show, Typeable)

instance Exception AddError

type Add m
  =  forall e
   . (e `CouldBe` AddError)
  => AccountNew
  -> ExceptT (Variant e) m AccountId


newtype AccountRepo m = AccountRepo {
        add :: Add m
}



