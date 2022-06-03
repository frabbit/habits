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

module Habits.UseCases.RegisterSpec (spec) where

import qualified Habits.Domain.AccountNew as AN
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AccountRepo.Class as ARC
import Habits.Domain.EmailAlreadyUsedError (EmailAlreadyUsedError (..))
import Habits.Domain.Password (Password (Password, unPassword))
import Habits.Domain.PasswordHash (PasswordHash (PasswordHash), isValid)
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import Habits.Test.Prelude
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.Class as RC
import qualified Habits.UseCases.Register.Live as RL
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Test.Hspec.Expectations.Lifted (shouldBe, shouldNotBe)
import Utils (catchAllToFail, expectError, sampleIO)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH

type Env m = CE.MkSorted '[R.Register m, AR.AccountRepo m]

envLayer :: forall m n. (MonadIO n, MonadIO m, _) => CE.ReaderCE '[] n (Env m)
envLayer = RL.mkLive <<-&& ARM.mkAccountRepoMemory

AppTH.mkBoilerplate "runApp" ''Env

runWithEnv :: _ b -> IO b
runWithEnv app = do
  env <- runReaderT envLayer CE.empty
  runApp env app

accountNewToRegisterRequest :: AN.AccountNew -> Password -> R.RegisterRequest
accountNewToRegisterRequest an pass = R.RegisterRequest {name = an.name, email = an.email, password = pass}

spec :: Spec
spec = describe "Register should" $ do
  let wrap :: _ => _
      wrap = runWithEnv . evalE . catchAllToFail
      propWrap :: _ => _
      propWrap f = property $ \x -> wrap (f x)
  it "succeed when creating a new account which email does not exist yet." . propWrap $ \rr -> S.do
    resp <- RC.register rr
    account <- ARC.getById resp.accountId
    S.coerce $ account.email `shouldBe` rr.email
    S.coerce $ account.name `shouldBe` rr.name
  it "lead to an unconfirmed email address" . propWrap $ \rr -> S.do
    resp <- RC.register rr
    account <- ARC.getById resp.accountId
    S.coerce $ account.emailConfirmed `shouldBe` False
  it "encode the password and store it encrypted." . propWrap $ \rr -> S.do
    resp <- RC.register rr
    account <- ARC.getById resp.accountId

    S.coerce $ account.password `shouldNotBe` PasswordHash (rr.password & unPassword)
  it "encode the password and store it encrypted." . propWrap $ \rr -> S.do
    accountId <- RC.register rr <&> (.accountId)
    account <- ARC.getById accountId
    S.coerce $ isValid rr.password account.password `shouldBe` True
  it "fail to register a new account with an existing email." . propWrap $ \an ->
    S.do
      ARC.add an
      RC.register (accountNewToRegisterRequest an (Password "pw"))
      & expectError @EmailAlreadyUsedError
