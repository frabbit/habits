{-# HLINT ignore "Use >>" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Use let" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Habits.UseCases.RegisterSpec where

import Control.Lens ((^.))

import Data.Function ((&))
import Habits.App
  ( runAppE, App (App),
  )
import Habits.AppEnv
  ( mkAppEnv,
  )
import Habits.Domain.Email (Email (..))
import Habits.Domain.Password (Password (Password))
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.Class as RC
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Haskus.Utils.Variant.Excepts (Excepts, liftE)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Utils (catchToFail, sampleIO)
import qualified Habits.Domain.AccountRepo.Class as ARC
import Habits.UseCases.Register (RegisterResponse(..))
import qualified Habits.UseCases.Register as Reg
import qualified Habits.Domain.Account as A
import Habits.Domain.Account (_accountId)
import qualified Habits.Domain.AccountRepo as AR
import Habits.Domain.AccountNew (AccountNew(AccountNew))
import qualified Habits.Domain.AccountNew as AN
import qualified Haskus.Utils.Variant.Excepts.Syntax as S

runWithEnv :: forall a . Excepts '[] App a -> IO a
runWithEnv app = do
  env <- mkAppEnv
  runAppE env app

spec :: Spec
spec = describe "RegisterSpec execute should" $ do
  it "return with success" . runWithEnv $
    let
      app :: Excepts '[ R.RegisterError, AR.RepositoryError, AR.AccountNotFoundError ] App ()
      app = do
        _ :: AccountNew <- sampleIO
        let accNew = AN.AccountNew { AN._name = "Peter", AN._email = Email "abc@de.de", AN._password = Password "abc" }
        RegisterResponse { Reg._accountId } <- liftE $ RC.execute R.RegisterRequest {R._name = "Peter", R._email = Email "abc@de.de", R._password = Password "abc"}
        account <- liftE $ ARC.getById _accountId
        A.toAccountNew account `shouldBe` accNew
    in
    app
    & catchToFail @R.RegisterError
    & catchToFail @AR.RepositoryError
    & catchToFail @AR.AccountNotFoundError
