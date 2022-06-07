{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Habits.UseCases.ConfirmEmailSpec (spec) where

import Habits.Test.Prelude

import Habits.Domain.EmailConfirmationNotFoundError (EmailConfirmationNotFoundError)

import qualified Habits.UseCases.ConfirmEmail as ConfirmEmail
import Habits.UseCases.ConfirmEmail.Class (ConfirmEmailM (confirmEmail))
import qualified Habits.UseCases.ConfirmEmail.Live as ConfirmEmailLive
import Habits.UseCases.ConfirmEmail.ConfirmEmailRequest (ConfirmEmailRequest (ConfirmEmailRequest))
import Utils (catchAllToFail, expectError)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Habits.Domain.AccountNew (AccountNew)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Veins.Test.QuickCheck (sampleIO)
import Habits.Domain.AccountRepo (AccountRepo)
import Habits.Domain.AccountRepo.Class (add, getById)
import Habits.Infra.Memory.AccountRepoMemory (mkAccountRepoMemory)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Habits.Infra.Memory.EmailConfirmationRepoMemory (mkEmailConfirmationRepoMemory)
import qualified Habits.Domain.EmailConfirmationRepo.Class as EmailConfirmationRepo
import Habits.Domain.EmailConfirmationRepo (EmailConfirmationRepo)

type Env m = CE.MkSorted '[ConfirmEmail.ConfirmEmail m, AccountRepo m, EmailConfirmationRepo m]

newtype AccountNewConfirmed = AccountNewConfirmed AccountNew
instance Arbitrary AccountNewConfirmed where arbitrary = AccountNewConfirmed <$> (arbitrary <&> (\a -> a{emailConfirmed = True}))

newtype AccountNewUnconfirmed = AccountNewUnconfirmed AccountNew
instance Arbitrary AccountNewUnconfirmed where arbitrary = AccountNewUnconfirmed <$> (arbitrary <&> (\a -> a{emailConfirmed = False}))

envLayer :: forall m n. (MonadIO n, _) => CE.ReaderCE '[] n (Env m)
envLayer =
  ConfirmEmailLive.mkLive <<-&& mkAccountRepoMemory <<-&& mkEmailConfirmationRepoMemory

AppTH.mkBoilerplate "runApp" ''Env

runWithEnv :: _ -> _ b -> IO b
runWithEnv layer app = do
  env <- runReaderT layer CE.empty
  runApp env app

embed :: _ => _
embed = runWithEnv (envLayer :: _) . evalE . catchAllToFail

spec :: Spec
spec = describe "ConfirmEmail should" $ do
  it "fail with EmailConfirmationNotFoundError when corresponding email confirmation request does not exist" . property $ \nonce -> embed $
    S.do
      confirmEmail (ConfirmEmailRequest nonce)
      & expectError @EmailConfirmationNotFoundError
  it "confirm the email address when corresponding email confirmation request exists" . property $ \(cfr) -> embed $
    S.do
      AccountNewUnconfirmed an <- S.coerce sampleIO
      accountId <- add an
      _ <- EmailConfirmationRepo.add $ cfr{accountId}
      confirmEmail (ConfirmEmailRequest cfr.emailConfirmationNonce)
      a <- getById accountId
      S.coerce $ a.emailConfirmed `shouldBe` True
      S.coerce $ a.email `shouldBe` cfr.email

