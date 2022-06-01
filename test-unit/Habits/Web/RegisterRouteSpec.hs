module Habits.Web.RegisterRouteSpec where

import Habits.Test.Prelude
import qualified Control.Lens as L

import Habits.UseCases.Register (unRegisterL)
import qualified Habits.UseCases.Register as R
import Habits.Web.Routes.RegisterRoute (fromDomain, registerRoute, setEmail, setPassword, toDomain)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Veins.Control.Lens.Utils (makeLensesWithSuffixL)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Servant (err400, err409, err500)
import Habits.Domain.EmailAlreadyUsedError (EmailAlreadyUsedError(EmailAlreadyUsedError))
import Habits.Domain.RepositoryError (RepositoryError(RepositoryError))
import qualified Veins.Data.HList as HL
import Data.Validation (Validation(Success))

type Env m = CE.MkSorted '[R.Register m]

data Mocks m = Mocks
  { register :: R.Register m
  }

defaultMocks :: forall m. Mocks m
defaultMocks =
  Mocks
    { register = mockify R.Register
    }

makeLensesWithSuffixL ''Mocks

envLayer :: forall m n. (MonadIO n) => Mocks m -> CE.ReaderCE '[] n (Env m)
envLayer mocks = pure $ CE.singleton mocks.register

AppTH.mkBoilerplate "runApp" ''Env

runWithEnv :: _ -> _ b -> IO b
runWithEnv mocks app = do
  env <- runReaderT (envLayer mocks) CE.empty
  runApp env app

setRegister :: _
setRegister = L.over (registerL . unRegisterL)

spec :: Spec
spec = describe "registerRoute should" $ do
  let wrap = runWithEnv
  it "return the response from Register service converted to Dto" . property $ \(rs,i) -> do
    let mocks = defaultMocks & setRegister (mockReturn $ pure rs)
    wrap mocks $ do
      out <- runExceptT $ registerRoute i
      out `shouldBe` Right (fromDomain rs)
  it "pass the request converted from Dto to Register service" . property $ \(rs,i) -> do
    (spy, mocks) <- defaultMocks & setRegister (mockReturn $ pure rs) & withSpy (registerL . unRegisterL)
    wrap mocks $ do
      runExceptT $ registerRoute i
      Success i' <- pure $ toDomain i
      let expected = HL.HCons i' HL.HNil
      args <- getSpyArgsIO spy
      args `shouldBe` [expected]
  it "fail with 400 when email is invalid" . property $ \rs -> wrap defaultMocks $ do
      let rr = rs & setEmail "invalid email"
      out <- runExceptT $ registerRoute rr
      out `shouldBe` Left err400
  it "fail with 400 when password is invalid" . property $ \rs -> wrap defaultMocks $ do
      out <- runExceptT $ registerRoute (rs & setPassword "")
      out `shouldBe` Left err400
  it "fail with 400 when email is already used" . propertyOne $ \rs -> do
    let mocks = defaultMocks & setRegister (mockReturn $ throwE EmailAlreadyUsedError)
    wrap mocks $ do
      out <- runExceptT $ registerRoute rs
      out `shouldBe` Left err409
  it "fail with 500 on repository error" . propertyOne $ \rs -> do
    let mocks = defaultMocks & setRegister (mockReturn $ throwE RepositoryError)
    wrap mocks $ do
      out <- runExceptT $ registerRoute rs
      out `shouldBe` Left err500