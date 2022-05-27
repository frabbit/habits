module Habits.Web.RegisterRouteSpec where

import qualified Control.Lens as L
import Control.Monad.Except (runExceptT, MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import Habits.UseCases.Register (executeL)
import qualified Habits.UseCases.Register as R
import Habits.Web.Routes.RegisterRoute (fromDomain, registerRoute, setEmail, setPassword, toDomain)
import Test.Hspec (Spec, describe, it, fit)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Veins.Control.Lens.Utils (makeLensesWithSuffixL)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Veins.Test.Mock (mockReturn, mockify, mkSpyIO, withSpy, getSpyCallsIO, getSpyArgsIO, SpyContext, getSpyArgs)
import Veins.Test.QuickCheck (propertyOne)
import Prelude
import Servant (err400, err409, err500)
import Test.QuickCheck (property, withMaxSuccess, Testable, Property)
import Habits.Domain.EmailAlreadyUsedError (EmailAlreadyUsedError(EmailAlreadyUsedError))
import Haskus.Utils.Variant.Excepts (failureE, liftE, successE)
import Habits.Domain.RepositoryError (RepositoryError(RepositoryError))
import qualified Veins.Data.HList as HL
import Utils (shouldBeIO)
import Data.Validation (Validation(Success))
import UnliftIO (TVar)

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

envLayer :: forall m n. (MonadIO n, MonadIO m, _) => Mocks m -> ReaderT (CE.ComposableEnv '[]) n (Env m)
envLayer mocks = pure $ CE.empty & CE.insert mocks.register

AppTH.mkBoilerplate "runApp" ''Env

runWithEnv :: _ -> _ b -> IO b
runWithEnv mocks app = do
  env <- runReaderT (envLayer mocks) CE.empty
  runApp env app



spec :: Spec
spec = describe "registerRoute should" $ do
  let wrap = runWithEnv
  it "return the response from Register service converted to Dto" . property $ \(rs,i) -> do
    let mocks = defaultMocks & L.over (registerL . executeL) (mockReturn $ pure rs)
    wrap mocks $ do
      out <- runExceptT $ registerRoute i
      out `shouldBe` Right (fromDomain rs)
  it "pass the request converted from Dto to Register service" . property $ \(rs,i) -> do
    (spy, mocks) <- defaultMocks & L.over (registerL . executeL) (mockReturn $ pure rs) & withSpy (registerL . executeL)
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
    let mocks = defaultMocks & L.over (registerL . executeL) (mockReturn $ (liftE . failureE) EmailAlreadyUsedError)
    wrap mocks $ do
      out <- runExceptT $ registerRoute rs
      out `shouldBe` Left err409
  it "fail with 500 on repository error" . propertyOne $ \rs -> do
    let mocks = defaultMocks & L.over (registerL . executeL) (mockReturn $ (liftE . failureE) RepositoryError)
    wrap mocks $ do
      out <- runExceptT $ registerRoute rs
      out `shouldBe` Left err500