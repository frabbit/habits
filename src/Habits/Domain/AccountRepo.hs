{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Habits.Domain.AccountRepo where

import           Habits.Domain.AccountId        ( AccountId )
import           Habits.Domain.AccountNew      as AccountNew
                                                ( AccountNew )

import           Control.Monad.Trans.Except     ( ExceptT )
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Monad.Exception (Throws)

import qualified Haskus.Utils.Variant.Excepts as Excepts
import Haskus.Utils.Variant.Excepts (Excepts(..))

data AddError = AddError deriving (Show, Typeable)

instance Exception AddError

type Add m = AccountNew -> Excepts '[AddError] m  AccountId


newtype AccountRepo m = AccountRepo {
        add :: Add m
}



