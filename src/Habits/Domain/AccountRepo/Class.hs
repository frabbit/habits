{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
                                                , AddW(unAddW)
                                                , GetById
                                                , GetByIdW(unGetByIdW)
                                                )
import qualified Veins.Data.HList as HL
import Language.Haskell.TH (Overlap(Overlappable))

class AccountRepo m where
        add :: Add m
        getById :: GetById m



instance (Monad m, MonadReader env m , Has (AR.AccountRepo m) env) => AccountRepo m where
  add x = do
    accountRepo <- lift $ asks getter
    unAddW (accountRepo ^. AR.add) x

  getById x = do
    (accountRepo :: AR.AccountRepo m) <- lift $ asks getter
    unGetByIdW (accountRepo ^. AR.getById) x
