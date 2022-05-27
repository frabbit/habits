module Habits.Web.LoginRouteSpec where

import qualified Control.Lens as L
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import Data.Validation (Validation (Success))
import Habits.Domain.AccountNotFoundError (AccountNotFoundError (AccountNotFoundError))
import Habits.Domain.PasswordIncorrectError (PasswordIncorrectError (PasswordIncorrectError))
import Habits.Domain.RepositoryError (RepositoryError (RepositoryError))
import Habits.UseCases.Login (executeL)
import qualified Habits.UseCases.Login as L
import Habits.Web.Routes.LoginRoute (fromDomain, loginRoute, setEmail, setPassword, toDomain)
import Haskus.Utils.Variant.Excepts (failureE, liftE)
import Servant (err400, err401, err500)
import Test.Hspec (Spec, fdescribe, it)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Test.QuickCheck (property)
import Veins.Control.Lens.Utils (makeLensesWithSuffixL)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Data.HList as HL
import qualified Veins.Test.AppTH as AppTH
import Veins.Test.Mock (getSpyArgsIO, mockReturn, mockify, withSpy)
import Veins.Test.QuickCheck (propertyOne)
import Prelude

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

envLayer :: forall m n. (MonadIO n, MonadIO m, _) => Mocks m -> ReaderT (CE.ComposableEnv '[]) n (Env m)
envLayer mocks = pure $ CE.empty & CE.insert mocks.login

AppTH.mkBoilerplate "runApp" ''Env

run :: _ -> _ b -> IO b
run mocks app = do
  env <- runReaderT (envLayer mocks) CE.empty
  runApp env app

spec :: Spec
spec = fdescribe "loginRoute should" $ do
  it "return the converted response from Login service" . property $ \(rs, i) -> do
    let mocks = defaultMocks & L.over (loginL . executeL) (mockReturn $ pure rs)
    run mocks $ do
      out <- runExceptT $ loginRoute i
      out `shouldBe` Right (fromDomain rs)
  it "pass the converted request to Login service" . property $ \(rs, i) -> do
    (spy, mocks) <- defaultMocks & L.over (loginL . executeL) (mockReturn $ pure rs) & withSpy (loginL . executeL)
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
    let mocks = defaultMocks & L.over (loginL . executeL) (mockReturn . liftE . failureE $ RepositoryError)
    run mocks $ do
      out <- runExceptT $ loginRoute rs
      out `shouldBe` Left err500
  it "fail with 401 on password incorrect error" . propertyOne $ \rs -> do
    let mocks = defaultMocks & L.over (loginL . executeL) (mockReturn . liftE . failureE $ PasswordIncorrectError)
    run mocks $ do
      out <- runExceptT $ loginRoute rs
      out `shouldBe` Left err401
  it "fail with 400 on account not found error " . propertyOne $ \rs -> do
    let mocks = defaultMocks & L.over (loginL . executeL) (mockReturn . liftE . failureE $ AccountNotFoundError)
    run mocks $ do
      out <- runExceptT $ loginRoute rs
      out `shouldBe` Left err400