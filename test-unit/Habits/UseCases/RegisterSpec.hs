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
import Habits.Infra.Memory.EmailServiceMemory (mkEmailServiceMemory)
import Habits.Infra.VarStorage.Live (mkVarStorageLive)
import Habits.Test.Prelude
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.Class as RC
import qualified Habits.UseCases.Register.Live as RL
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Test.Hspec.Expectations.Lifted (shouldBe, shouldNotBe, shouldSatisfy)
import Utils (catchAllToFail, expectError)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Habits.Domain.EmailMessage (EmailMessage, EmailReceiver (EmailReceiver))
import Habits.Infra.VarStorage.Class (readVar)
import Habits.Infra.VarStorage (VarStorage)
import Habits.Domain.EmailConfirmationRepo (EmailConfirmationRepo)
import Habits.Domain.EmailConfirmationRepo.Class (getByNonceOrFail)

import Habits.Infra.Memory.EmailConfirmationRepoMemory (mkEmailConfirmationRepoMemory)
import Habits.Domain.Emails.RegistrationEmail (getNonceFromRegistrationEmail)

type Env m = CE.MkSorted '[R.Register m, AR.AccountRepo m, VarStorage [EmailMessage] m, EmailConfirmationRepo m]

envLayer :: forall m n. (MonadIO n, MonadIO m, _) => CE.ReaderCE '[] n (Env m)
envLayer =
  RL.mkLive
    <<-&& ARM.mkAccountRepoMemory
    <<- mkEmailServiceMemory
    <<-&& mkVarStorageLive @[EmailMessage] []
    <<-&& mkEmailConfirmationRepoMemory

AppTH.mkBoilerplate "runApp" ''Env

runWithEnv :: _ b -> IO b
runWithEnv app = do
  env <- CE.runReaderCE envLayer
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
  it "create an email confirmation entry which is accessible by using the nonce inside of the registration email" . propWrap $ \rr -> S.do
    RC.register rr
    [msg] <- S.lift $ readVar @[EmailMessage]
    Just nonce <- S.pure $ getNonceFromRegistrationEmail msg.body
    emailConf <- getByNonceOrFail nonce
    S.liftIO $ emailConf.email `shouldBe` rr.email
  it "send an email to confirm the email address" . propWrap $ \rr -> S.do
    resp <- RC.register rr
    ARC.getById resp.accountId
    messages <- S.lift $ readVar @[EmailMessage]
    S.coerce $ messages `shouldSatisfy` (not . null)
    [msg] <- S.pure messages
    S.liftIO $ msg.receiver `shouldBe` EmailReceiver rr.email
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
