{-# LANGUAGE AllowAmbiguousTypes #-}
module Habits.Web.RegisterRouteSpec where

import qualified Control.Lens as L
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import Habits.UseCases.Register (executeL)
import qualified Habits.UseCases.Register as R
import Habits.Web.Routes.RegisterRoute (fromDomain, registerRoute, setEmail, setPassword)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Veins.Control.Lens.Utils (makeLensesWithSuffixL)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Veins.Test.Mock (mockReturn, mockify)
import Prelude
import Servant (err400)
import Test.QuickCheck (property, withMaxSuccess, Testable, Property)

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


propertyRuns :: Testable a => Int -> a -> Property
propertyRuns n = withMaxSuccess n . property


spec :: Spec
spec = describe "registerRoute should" $ do
  let wrap = runWithEnv
  it "return the response from Register service converted to Dto" . property $ \(rs,i) -> do
    let mocks = defaultMocks & L.over (registerL . executeL) (mockReturn $ pure rs)
    wrap mocks $ do
      out <- runExceptT $ registerRoute i
      out `shouldBe` Right (fromDomain rs)
  it "return the response from Register service converted to Dto" . property $ \(rs,i) -> do
    let mocks = defaultMocks & L.over (registerL . executeL) (mockReturn $ pure rs)
    wrap mocks $ do
      out <- runExceptT $ registerRoute i
      out `shouldBe` Right (fromDomain rs)
  it "fail with 400 when email is invalid" . property $ \rs -> wrap defaultMocks $ do
      let rr = rs & setEmail "invalid email"
      out <- runExceptT $ registerRoute rr
      out `shouldBe` Left err400
  it "fail with 400 when password is invalid" . property $ \rs -> wrap defaultMocks $ do
      out <- runExceptT $ registerRoute (rs & setPassword "")
      out `shouldBe` Left err400