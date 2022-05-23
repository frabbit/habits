{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Habits.UseCases.LoginSpec where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))

import Haskus.Utils.Variant.Excepts (evalE)
import Test.Hspec
  ( Spec,
    describe,
    it, focus,
  )
import qualified Veins.Test.AppTH as AppTH
import qualified Veins.Data.ComposableEnv as CE
import qualified Habits.Domain.AccountRepo as AR
import qualified Habits.Domain.AccountRepo.Class as ARC
import qualified Habits.Infra.Memory.AccountRepoMemory as ARM
import qualified Habits.UseCases.Register as R
import qualified Habits.UseCases.Register.Live as RL

import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Utils (catchAllToFail, sampleIO, expectError)
import qualified Habits.UseCases.Login as Login
import qualified Habits.UseCases.Login.Class as LC
import qualified Habits.UseCases.Login.Live as LoginLive
import Habits.Domain.PasswordHash (mkFromPassword)
import qualified Control.Lens as L
import qualified Habits.Domain.AccountNew as AN
import Habits.Domain.Password (Password(..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Lens ((^.))
import Habits.UseCases.Login.LoginRequest (LoginRequest(EmailPasswordLoginRequest))
import Habits.UseCases.Login.LoginResponse (LoginResponse(..))
import Test.Hspec.Expectations.Lifted (shouldBe)
import Habits.Domain.AccountNotFoundError (AccountNotFoundError)
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError)
import Veins.Test.HSpec.TH (shouldMatchPattern, ShouldMatchPattern (shouldMatchPattern1))
import Habits.Domain.AccessToken (AccessToken(AccessToken))
import Habits.Domain.RefreshToken (RefreshToken(RefreshToken))
import Data.Text (Text)
import qualified Habits.UseCases.Login.LoginResponse as LR
import qualified Habits.Domain.AccessToken as AT
import qualified Habits.Domain.AccessTokenSecret as ATS
import qualified Habits.Domain.RefreshTokenSecret as RTS
import qualified Habits.Domain.RefreshToken as RT

type Env m = CE.MkSorted '[R.Register m, AR.AccountRepo m, Login.Login m]

envLayer :: forall m n. (MonadIO n, ARC.AccountRepo m, MonadIO m, _) => ReaderT (CE.ComposableEnv '[]) n (Env m)
envLayer = ARM.mkAccountRepoMemory `CE.provideAndChainLayer` RL.mkLive `CE.provideAndChainLayer` LoginLive.mkLive

AppTH.mkBoilerplate "runApp" ''Env

addUserWithPassword :: _ => _
addUserWithPassword = S.do
  pw <- S.coerce sampleIO
  pwHash <- S.coerce $ mkFromPassword pw
  acc <- S.coerce $ sampleIO <&> L.set AN.password pwHash
  AR.add acc
  S.pure (acc, pw)

runWithEnv :: _ b -> IO b
runWithEnv app = do
  env <- runReaderT envLayer CE.empty
  runApp env app

spec :: Spec
spec = focus . describe "Login.execute should" $ do
  let runEval = runWithEnv . evalE
  it "be successfull when account with same password and email exists" . runEval . catchAllToFail $ S.do
    (acc, pw) <- addUserWithPassword

    resp <- LC.execute $ EmailPasswordLoginRequest (acc ^. AN.email) pw
    S.coerce $ $('resp `shouldMatchPattern1` [p|LoginResponse (AccessToken _) (RefreshToken _)|])
  it "return a valid and not expired access token" . runEval . catchAllToFail $ S.do
    (acc, pw) <- addUserWithPassword
    resp <- LC.execute $ EmailPasswordLoginRequest (acc ^. AN.email) pw
    let verifyResult = AT.verifyAccessToken (ATS.mkAccessTokenSecret "abc") (resp ^. LR.accessToken)
    S.coerce $ verifyResult `shouldBe` True
  it "return a valid and not expired refresh token" . runEval . catchAllToFail $ S.do
    (acc, pw) <- addUserWithPassword
    resp <- LC.execute $ EmailPasswordLoginRequest (acc ^. AN.email) pw
    let verifyResult = RT.verifyRefreshToken (RTS.mkRefreshTokenSecret "abc") (resp ^. LR.refreshToken)
    S.coerce $ verifyResult `shouldBe` True
  it "fail with AccountNotFoundError when account with given email does not exist" . runEval . catchAllToFail $ S.do
    pw <- S.coerce sampleIO
    email <- S.coerce sampleIO
    LC.execute $ EmailPasswordLoginRequest email pw
    & expectError @AccountNotFoundError
  it "fail with PasswordIncorrectError when account with email exists but password is wrong" . runEval . catchAllToFail $ S.do
    let pw = Password "InvalidPassword"
    acc <- S.coerce sampleIO
    AR.add acc
    LC.execute $ EmailPasswordLoginRequest (acc ^. AN.email) pw
    & expectError @PasswordIncorrectError

