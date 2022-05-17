{-# HLINT ignore "Use >>" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Use let" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Habits.UseCases.RegisterSpec where

import Data.Function ((&))
import Habits.Domain.Email (Email (..))
import Habits.Domain.Password (Password (Password))
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.Class as RC
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Haskus.Utils.Variant.Excepts (Excepts, liftE, evalE)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Utils (catchToFail, sampleIO)
import qualified Habits.Domain.AccountRepo.Class as ARC
import Habits.UseCases.Register (RegisterResponse(..))
import qualified Habits.UseCases.Register as Reg
import qualified Habits.Domain.Account as A
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AccountNew as AN
import qualified Veins.Data.ComposableEnv as CE
import Control.Monad.IO.Class (MonadIO)
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.UseCases.Register.Live as RL
import qualified Veins.Test.AppTH as AppTH

type Env m = CE.MkSorted '[R.Register m, AR.AccountRepo m]

mkAppEnv :: forall n m . (MonadIO n, ARC.AccountRepo m, MonadIO m) => n (Env m)
mkAppEnv = do
  accountRepo <- ARM.mkAccountRepoMemory
  pure $ CE.empty & CE.insert RL.mkLive & CE.insert accountRepo

AppTH.mkBoilerplate "runApp" ''Env

runWithEnv :: _ b -> IO b
runWithEnv app = do
  env <- mkAppEnv
  runApp env app

spec :: Spec
spec = describe "RegisterSpec execute should" $ do
  it "return with success" . runWithEnv . evalE $
    let
      app :: Excepts '[ R.RegisterError, AR.RepositoryError, AR.AccountNotFoundError ] _ ()
      app = do
        _ :: AN.AccountNew <- sampleIO
        let accNew = AN.AccountNew { AN._name = "Peter", AN._email = Email "abc@de.de", AN._password = Password "abc" }
        RegisterResponse { Reg._accountId } <- liftE $ RC.execute R.RegisterRequest {R._name = "Peter", R._email = Email "abc@de.de", R._password = Password "abc"}
        account <- liftE $ ARC.getById _accountId
        A.toAccountNew account `shouldBe` accNew
    in
    app
    & catchToFail @R.RegisterError
    & catchToFail @AR.RepositoryError
    & catchToFail @AR.AccountNotFoundError
