{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Habits.UseCases.Register where
import           Habits.Domain.Email            ( Email(Email) )

import           Control.Monad.Exception        ( Throws )
import           Control.Monad.Trans.Except     ( ExceptT )
import           Data.Text                      ( Text )
import           Habits.Domain.AccountRepo      ( AddError )

import           Control.Exception              ( Exception )
import           Data.Typeable                  ( Typeable )
import           Haskus.Utils.Variant.Excepts   ( Excepts )

data RegisterError = RegisterError
  deriving (Show, Typeable)

instance Exception RegisterError


data RegisterRequest = RegisterRequest
  { email :: Email
  , name  :: Text
  }

newtype RegisterResponse = RegisterResponse {
  success::Bool
} deriving (Show, Eq, Ord)

type RegisterExec m
  = RegisterRequest -> Excepts '[RegisterError] m RegisterResponse

newtype Register m = Register {
  execute :: RegisterExec m
}
