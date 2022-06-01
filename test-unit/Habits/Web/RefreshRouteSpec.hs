module Habits.Web.RefreshRouteSpec where

import qualified Control.Lens as L
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import Data.Validation (Validation (Success))
import Habits.Domain.RepositoryError (RepositoryError (RepositoryError))
import Habits.UseCases.Refresh (unRefreshL)
import qualified Habits.UseCases.Refresh as R
import Habits.Web.Routes.RefreshRoute (fromDomain, refreshRoute, toDomain, setRefreshToken)
import Haskus.Utils.Variant.Excepts (failureE, liftE)
import Servant (err400, err401, err500)
import Test.Hspec (Spec, fdescribe, it, describe)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Test.QuickCheck (property)
import Veins.Control.Lens.Utils (makeLensesWithSuffixL)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Data.HList as HL
import qualified Veins.Test.AppTH as AppTH
import Veins.Test.Mock (getSpyArgsIO, mockReturn, mockify, withSpy)
import Veins.Test.QuickCheck (propertyOne)
import Prelude
import Habits.Domain.RefreshTokenExpiredError (RefreshTokenExpiredError(RefreshTokenExpiredError))
import Habits.Domain.RefreshTokenInvalidError (RefreshTokenInvalidError(RefreshTokenInvalidError))
import Habits.Domain.RefreshTokenIssuedNotFoundError (RefreshTokenIssuedNotFoundError(RefreshTokenIssuedNotFoundError))

type Env m = CE.MkSorted '[R.Refresh m]

data Mocks m = Mocks
  { refresh :: R.Refresh m
  }

defaultMocks :: forall m. Mocks m
defaultMocks =
  Mocks
    { refresh = mockify R.Refresh
    }

makeLensesWithSuffixL ''Mocks

envLayer :: forall m n. (MonadIO n) => Mocks m -> CE.ReaderCE '[] n (Env m)
envLayer mocks = pure $ CE.empty & CE.insert mocks.refresh

AppTH.mkBoilerplate "runApp" ''Env

run :: _ -> _ b -> IO b
run mocks app = do
  env <- runReaderT (envLayer mocks) CE.empty
  runApp env app

spec :: Spec
spec = fdescribe "refreshRoute should" $ do
  it "return the converted response from Refresh service" . property $ \(rs, i) -> do
    let mocks = defaultMocks & L.over (refreshL . unRefreshL) (mockReturn $ pure rs)
    run mocks $ do
      out <- runExceptT $ refreshRoute i
      out `shouldBe` Right (fromDomain rs)
  it "pass the converted request to Refresh service" . property $ \(rs, i) -> do
    (spy, mocks) <- defaultMocks & L.over (refreshL . unRefreshL) (mockReturn $ pure rs) & withSpy (refreshL . unRefreshL)
    run mocks $ do
      runExceptT $ refreshRoute i
      Success i' <- pure $ toDomain i
      let expected = HL.HCons i' HL.HNil
      args <- getSpyArgsIO spy
      args `shouldBe` [expected]
  it "fail with 400 when token is empty" . property $ \rs -> run defaultMocks $ do
    let rr = rs & setRefreshToken ""
    out <- runExceptT $ refreshRoute rr
    out `shouldBe` Left err400
  it "fail with 500 on repository error" . propertyOne $ \rs -> do
    let mocks = defaultMocks & L.over (refreshL . unRefreshL) (mockReturn . liftE . failureE $ RepositoryError)
    run mocks $ do
      out <- runExceptT $ refreshRoute rs
      out `shouldBe` Left err500
  it "fail with 401 on RefreshTokenExpiredError" . propertyOne $ \rs -> do
    let mocks = defaultMocks & L.over (refreshL . unRefreshL) (mockReturn . liftE . failureE $ RefreshTokenExpiredError)
    run mocks $ do
      out <- runExceptT $ refreshRoute rs
      out `shouldBe` Left err401
  it "fail with 401 on RefreshTokenInvalidError" . propertyOne $ \rs -> do
    let mocks = defaultMocks & L.over (refreshL . unRefreshL) (mockReturn . liftE . failureE $ RefreshTokenInvalidError)
    run mocks $ do
      out <- runExceptT $ refreshRoute rs
      out `shouldBe` Left err401
  it "fail with 401 on RefreshTokenIssuedNotFoundError" . propertyOne $ \rs -> do
    let mocks = defaultMocks & L.over (refreshL . unRefreshL) (mockReturn . liftE . failureE $ RefreshTokenIssuedNotFoundError)
    run mocks $ do
      out <- runExceptT $ refreshRoute rs
      out `shouldBe` Left err401