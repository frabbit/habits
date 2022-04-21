{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
module Habits.Domain.AccountRepo.Class where

import qualified Habits.Domain.AccountRepo     as AR

import           Control.Monad.Exception        ( Throws )
import           Control.Monad.RWS              ( MonadReader
                                                , MonadTrans(lift)
                                                , asks
                                                )
import           Data.Has                       ( Has
                                                , getter
                                                )
import           Habits.Domain.AccountRepo      ( Add
                                                , AddError
                                                )
import           Haskus.Utils.Variant.Excepts   ( liftE
                                                , toVariant
                                                )

class AccountRepo m where
        add :: Add m


instance (Monad m, MonadReader env m , Has (AR.AccountRepo m) env) => AccountRepo m where
  add x = do
    accountRepo <- lift $ asks getter
    let AR.AccountRepo { AR.add = add } = accountRepo
    add x
