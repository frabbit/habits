{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Habits.UseCases.RegisterSpec where

import           Control.Lens                   ( makeLenses )
import           Control.Monad.Error.Class      ( MonadError )
import           Control.Monad.Exception        ( CallTrace
                                                , EM
                                                , EMT(EMT)
                                                , Exception
                                                , Throws
                                                , runEMT
                                                , unEMT
                                                )
import           Control.Monad.Exception.Catch  ( MonadCatch(catch) )
import           Control.Monad.Exception.Throws ( CheckedException )
import           Control.Monad.RWS              ( MonadIO(liftIO)
                                                , asks
                                                )
import           Control.Monad.Reader           ( Reader
                                                , ReaderT
                                                , runReaderT
                                                )
import           Control.Monad.Reader.Class     ( MonadReader
                                                , ask
                                                , local
                                                )
import           Data.Has                       ( Has
                                                , getter
                                                , hasLens
                                                , modifier
                                                )
import           Data.Typeable                  ( Typeable )
import           Habits.Domain.AccountId        ( AccountId(AccountId) )
import           Habits.Domain.AccountNew       ( AccountNew )
import qualified Habits.Domain.AccountRepo     as AR
                                                ( AccountRepo(..)
                                                , Add
                                                , add
                                                )
import qualified Habits.UseCases.Register      as R

import           Habits.Domain.AccountRepo      ( AddError )
import qualified Habits.Domain.AccountRepo.Class
                                               as ARC
                                                ( AccountRepo
                                                , add
                                                )
import           Habits.Domain.AccountRepo.InMemory
                                               as AccountRepoInMemory
import           Habits.Domain.Email            ( Email(..) )
import           Habits.UseCases.Register       ( RegisterRequest(..)
                                                , RegisterResponse(..)
                                                , success
                                                )

import qualified Control.Exception

import           Control.Monad.Trans.Control    ( MonadBaseControl )
import           Data.Function                  ( (&) )
import           Habits.UseCases.Register.Class ( Register
                                                , execute
                                                )

import           Habits.Tools.MonadThrow        ( MonadThrow
                                                , throw
                                                )
import qualified Habits.UseCases.Register.Live as RegisterLive
import           Test.Hspec
import           Unsafe.Coerce                  ( unsafeCoerce )

import qualified Haskus.Utils.Variant.Excepts  as Excepts
import           Haskus.Utils.Variant.Excepts   ( Excepts(..) )

data AppEnv m = AppEnv
  { _accountRepo :: AR.AccountRepo m
  , _register    :: R.Register m
  }


newtype AppT env m a = AppT { unAppT :: ReaderT env m a } deriving (Functor, Applicative, Monad)

instance (Monad m) => MonadReader env (AppT env m) where
  ask = AppT ask
  local f ma = AppT $ local f (unAppT ma)

newtype App a = App { unApp :: AppT (AppEnv App) IO a } deriving (Functor, Applicative, Monad)

deriving via (AppT (AppEnv App) IO) instance MonadReader (AppEnv App) App

makeLenses ''AppEnv

runApp :: AppEnv App -> Excepts '[] App a -> IO a
runApp env app = runReaderWithEnv (unwrap app)
 where
  unwrap = unAppT . unApp . (Excepts.evalE)
  runReaderWithEnv r = runReaderT r env



--instance MonadReader (AppEnv e) (App e) where
--  ask = App . EMT $ fmap Right ask
--  local f ma = App . EMT $ local f (unEMT . unApp $ ma)

instance Has (AR.AccountRepo m) (AppEnv m) where
  hasLens = accountRepo

instance Has (R.Register m) (AppEnv m) where
  hasLens = register





spec :: Spec
spec = describe "RegisterSpec execute should" $ do
  it "return with success" $ do
    res <- runApp env $ Excepts.catchLiftLeft
      mapE
      (execute (RegisterRequest { name = "Peter", email = Email "abc@de.de" }))
    res `shouldBe` (RegisterResponse { success = True })
 where
  --mapE :: R.RegisterError -> Excepts '[] App RegisterResponse
  mapE (e::R.RegisterError) = pure (RegisterResponse { success = True })
  env :: AppEnv App
  env = AppEnv
    { _accountRepo = AR.AccountRepo { AR.add = \_ -> pure (AccountId "abc") }
    , _register    = R.Register { R.execute = RegisterLive.register }
    }
