{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Habits.Web.CreateHabitRouteSpec where

import Habits.Test.Prelude

import Test.Hspec.Expectations.Lifted (shouldBe)
import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Data.Kind (Type)
import Habits.Web.Routes.CreateHabitRoute (CreateHabitResponseDto(..), createHabitRoute)
import Habits.UseCases.CreateHabit (CreateHabit (CreateHabit), createHabitL)
import Veins.Control.Lens.Utils (makeLensesWithSuffixL)
import qualified Control.Lens as L
import Servant (ServerError (errHTTPCode))
import Data.Either.Extra (mapLeft)
import Test.QuickCheck (suchThat)

type Env (m :: Type -> Type) = CE.MkSorted '[CreateHabit m]

data Mocks m = Mocks
  { createHabitMock :: CreateHabit m
  }

defaultMocks :: forall m. Mocks m
defaultMocks =
  Mocks
    { createHabitMock = mockify CreateHabit
    }

makeLensesWithSuffixL ''Mocks

setCreateHabit :: _
setCreateHabit = L.over (createHabitMockL . createHabitL)

envLayer :: forall m n. (MonadIO n) => Mocks m -> CE.ReaderCE '[] n (Env m)
envLayer m = pure $ CE.singleton m.createHabitMock

AppTH.mkBoilerplate "runApp" ''Env

run :: _ -> _ b -> IO b
run mocks app = do
  env <- runReaderT (envLayer mocks) CE.empty
  runApp env app

spec :: Spec
spec = describe "createHabitRoute should" $ do
  it "return 401 when AccountId of authenticated account does not match the request's AccountId" . property $ \(a, req, res) -> do
    let mocks = defaultMocks & setCreateHabit (mockReturn $ pure res)
    run mocks $ do
      accountId <- liftIO $ generate $ arbitrary `suchThat` (/= a.accountId)
      out <- runExceptT $ createHabitRoute a (req{accountId})
      mapLeft errHTTPCode out `shouldBe` Left 401
  it "return 400 when AccountId is invalid" . property $ \(a, req, res) -> do
    let mocks = defaultMocks & setCreateHabit (mockReturn $ pure res)
    run mocks $ do
      out <- runExceptT $ createHabitRoute a req{accountId = "invalid"}
      mapLeft errHTTPCode out `shouldBe` Left 400
  it "return 400 when name is invalid" . property $ \(a, req, res) -> do
    let mocks = defaultMocks & setCreateHabit (mockReturn $ pure res)
    run mocks $ do
      out <- runExceptT $ createHabitRoute a req{name = ""}
      mapLeft errHTTPCode out `shouldBe` Left 400
  it "return a successful response containing the habitId of the created resource when everything is valid." . property $ \(a, req, res) -> do
    let mocks = defaultMocks & setCreateHabit (mockReturn $ pure res)
    run mocks $ do
      out <- runExceptT $ createHabitRoute a req{accountId = a.accountId}
      out `shouldBe` Right CreateHabitResponseDto{habitId = res.habitId.unHabitId}
