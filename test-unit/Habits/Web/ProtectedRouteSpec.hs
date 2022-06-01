module Habits.Web.ProtectedRouteSpec where

import Habits.Test.Prelude

import Test.Hspec.Expectations.Lifted (shouldBe)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Habits.Web.Routes.ProtectedRoute (protectedRoute, ProtectedResponseDto (ProtectedResponseDto, accountId))
import Data.Kind (Type)

type Env (m :: Type -> Type) = CE.MkSorted '[]

envLayer :: forall m n. (MonadIO n) => CE.ReaderCE '[] n (Env m)
envLayer = pure CE.empty

AppTH.mkBoilerplate "runApp" ''Env

run :: _ b -> IO b
run app = do
  env <- runReaderT envLayer CE.empty
  runApp env app

spec :: Spec
spec = fdescribe "protectedRoute should" $ do
  it "return response including accountId of AuthenticatedAccount" . property $ \a -> run $ do
    out <- runExceptT $ protectedRoute a
    out `shouldBe` Right ProtectedResponseDto{accountId = a.accountId}
