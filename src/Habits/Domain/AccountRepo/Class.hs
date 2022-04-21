{-# LANGUAGE UndecidableInstances #-}
module Habits.Domain.AccountRepo.Class where

import qualified Habits.Domain.AccountRepo     as AR

import           Control.Monad.RWS              ( MonadReader
                                                , MonadTrans(lift)
                                                , asks
                                                )
import           Data.Has                       ( Has
                                                , getter
                                                )
import           Habits.Domain.AccountRepo      ( Add
                                                )

class AccountRepo m where
        add :: Add m


instance (Monad m, MonadReader env m , Has (AR.AccountRepo m) env) => AccountRepo m where
  add x = do
    accountRepo <- lift $ asks getter
    let AR.AccountRepo { AR.add = add' } = accountRepo
    add' x
