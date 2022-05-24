{-# HLINT ignore "Use >>" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Use let" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Habits.UseCases.RegisterSpec where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Habits.Domain.Account as A
import qualified Habits.Domain.AccountNew as AN
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AccountRepo.Class as ARC
import Habits.Domain.EmailAlreadyUsedError (EmailAlreadyUsedError (..))
import Habits.Domain.Password (Password (Password, unPassword))
import Habits.Domain.PasswordHash (PasswordHash (PasswordHash), isValid)
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.Class as RC
import qualified Habits.UseCases.Register.Live as RL
import qualified Habits.UseCases.Register.RegisterRequest as RR
import Habits.UseCases.Register.RegisterResponse
  ( RegisterResponse (..),
  )
import qualified Habits.UseCases.Register.RegisterResponse as RegisterResponse
import Haskus.Utils.Variant.Excepts (evalE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Lifted (shouldBe, shouldNotBe)
import Utils (catchAllToFail, expectError, sampleIO)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH

type Env m = CE.MkSorted '[R.Register m, AR.AccountRepo m]

envLayer :: forall m n. (MonadIO n, ARC.AccountRepo m, MonadIO m, _) => ReaderT (CE.ComposableEnv '[]) n (Env m)
envLayer = ARM.mkAccountRepoMemory `CE.provideAndChainLayerFlipped` RL.mkLive

AppTH.mkBoilerplate "runApp" ''Env

runWithEnv :: _ b -> IO b
runWithEnv app = do
  env <- runReaderT envLayer CE.empty
  runApp env app

accountNewToRegisterRequest :: AN.AccountNew -> Password -> R.RegisterRequest
accountNewToRegisterRequest an pass = R.RegisterRequest {R._name = an.name, R._email = an.email, R._password = pass}

spec :: Spec
spec = describe "Register should" $ do
  let runEval = runWithEnv . evalE
  let wrap = runWithEnv . evalE . catchAllToFail
  it "succeed when creating a new account which email does not exist yet." . wrap $ S.do
    rr <- S.coerce sampleIO
    RegisterResponse {_accountId} <- RC.execute rr
    account <- ARC.getById _accountId
    S.coerce $ account.email `shouldBe` rr ^. RR.email
    S.coerce $ account.name `shouldBe` rr ^. RR.name
  it "encode the password and store it encrypted." . wrap $ S.do
    rr <- S.coerce sampleIO
    RegisterResponse {_accountId} <- RC.execute rr
    account <- ARC.getById _accountId

    S.coerce $ account.password `shouldNotBe` PasswordHash (rr ^. RR.password & unPassword)
  it "encode the password and store it encrypted." . wrap $ S.do
    rr <- S.coerce sampleIO
    accountId <- RC.execute rr <&> (^. RegisterResponse.accountId)
    account <- ARC.getById accountId
    S.coerce $ isValid (rr ^. RR.password) (account.password) `shouldBe` True

  it "fail when creating a new account which email does not exist yet." . runEval . catchAllToFail $
    S.do
      an :: AN.AccountNew <- S.coerce sampleIO
      ARC.add an
      RC.execute (accountNewToRegisterRequest an (Password "pw"))
      & expectError @EmailAlreadyUsedError
