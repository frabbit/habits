{-# LANGUAGE UndecidableInstances #-}
module Habits.Domain.AccountRepo.Class where

import qualified Habits.Domain.AccountRepo     as AR

import           Control.Lens                   ( (^.) )
import           Control.Monad.RWS              ( MonadReader
                                                , MonadTrans(lift)
                                                , asks
                                                )
import           Data.Has                       ( Has
                                                , getter
                                                )
import           Habits.Domain.AccountRepo      ( Add
                                                , GetById
                                                , WrapAdd(unWrapAdd)
                                                , WrapGetById(unWrapGetById)
                                                )

class AccountRepo m where
        add :: Add m
        getById :: GetById m


instance (Monad m, MonadReader env m , Has (AR.AccountRepo m) env) => AccountRepo m where
  add :: Add m
  add x = do
    accountRepo <- lift $ asks getter
    let f = accountRepo ^. AR.add
    unWrapAdd f x
  getById :: GetById m
  getById x = do
    (accountRepo :: AR.AccountRepo m) <- lift $ asks getter
    let f = accountRepo ^. AR.getById
    unWrapGetById f x
