module Habits.Web.LoginRouteSpec where

import Habits.Test.Prelude

import qualified Control.Lens as L
import Data.Validation (Validation (Success))
import Habits.Domain.AccountNotFoundError (AccountNotFoundError (AccountNotFoundError))
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError (PasswordIncorrectError))
import Habits.Domain.RepositoryError (RepositoryError (RepositoryError))
import Habits.UseCases.Login (unLoginL)
import qualified Habits.UseCases.Login as L
import Habits.Web.Routes.LoginRoute (fromDomain, loginRoute, setEmail, setPassword, toDomain)
import Servant (err400, err401, err500)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Veins.Control.Lens.Utils (makeLensesWithSuffixL)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Data.HList as HL
import qualified Veins.Test.AppTH as AppTH

type Env m = CE.MkSorted '[L.Login m]

data Mocks m = Mocks
  { login :: L.Login m
  }

defaultMocks :: forall m. Mocks m
defaultMocks =
  Mocks
    { login = mockify L.Login
    }

makeLensesWithSuffixL ''Mocks

envLayer :: forall m n. (MonadIO n) => Mocks m -> CE.ReaderCE '[] n (Env m)
envLayer mocks = pure $ CE.singleton mocks.login

AppTH.mkBoilerplate "runApp" ''Env

run :: _ -> _ b -> IO b
run mocks app = do
  env <- runReaderT (envLayer mocks) CE.empty
  runApp env app

setLogin :: _
setLogin = L.over (loginL . unLoginL)

spec :: Spec
spec = describe "loginRoute should" $ do
  it "return the converted response from Login service" . property $ \(rs, i) -> do
    let mocks = defaultMocks & setLogin (mockReturn $ pure rs)
    run mocks $ do
      out <- runExceptT $ loginRoute i
      out `shouldBe` Right (fromDomain rs)
  it "pass the converted request to Login service" . property $ \(rs, i) -> do
    (spy, mocks) <- defaultMocks & setLogin (mockReturn $ pure rs) & withSpy (loginL . unLoginL)
    run mocks $ do
      runExceptT $ loginRoute i
      Success i' <- pure $ toDomain i
      let expected = HL.HCons i' HL.HNil
      args <- getSpyArgsIO spy
      args `shouldBe` [expected]
  it "fail with 400 when email is invalid" . property $ \rs -> run defaultMocks $ do
    let rr = rs & setEmail "invalid email"
    out <- runExceptT $ loginRoute rr
    out `shouldBe` Left err400
  it "fail with 400 when password is invalid" . property $ \rs -> run defaultMocks $ do
    out <- runExceptT $ loginRoute (rs & setPassword "")
    out `shouldBe` Left err400
  it "fail with 500 on repository error" . propertyOne $ \rs -> do
    let mocks = defaultMocks & setLogin (mockReturn . throwE $ RepositoryError)
    run mocks $ do
      out <- runExceptT $ loginRoute rs
      out `shouldBe` Left err500
  it "fail with 401 on password incorrect error" . propertyOne $ \rs -> do
    let mocks = defaultMocks & setLogin (mockReturn . throwE $ PasswordIncorrectError)
    run mocks $ do
      out <- runExceptT $ loginRoute rs
      out `shouldBe` Left err401
  it "fail with 400 on account not found error " . propertyOne $ \rs -> do
    let mocks = defaultMocks & setLogin (mockReturn . throwE $ AccountNotFoundError)
    run mocks $ do
      out <- runExceptT $ loginRoute rs
      out `shouldBe` Left err400