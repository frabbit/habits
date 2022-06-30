{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# HLINT ignore "Use let" #-}

module Habits.Web.Routes.CreateHabitRoute where

import Habits.Prelude
import Data.Aeson (FromJSON, ToJSON)
import Habits.Web.Utils (mapAllErrorsToServerError)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Haskus.Utils.Variant.Excepts.Utils (fromValidation, toExceptT)
import Servant (ServerError)
import Servant.API (JSON, Post, ReqBody, type (:>), AuthProtect)
import Veins.Data.Codec (Encoder, errSimple)
import qualified Veins.Data.Codec as Codec
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)
import Habits.UseCases.CreateHabit.CreateHabitRequest (CreateHabitRequest (..))
import Habits.Domain.AccountId (accountIdFromText, unAccountId)
import Habits.UseCases.CreateHabit.CreateHabitResponse (CreateHabitResponse)
import Habits.UseCases.CreateHabit.Class (createHabit, CreateHabitM)
import Habits.Web.Auth (AuthenticatedAccount)
import Habits.Web.UnauthorizedError (UnauthorizedError(UnauthorizedError))
import qualified Data.Text as T
import Data.Validation (Validation(Failure))
import Test.QuickCheck (suchThat)

data CreateHabitResponseDto = CreateHabitResponseDto
  { habitId :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON CreateHabitResponseDto

instance FromJSON CreateHabitResponseDto

data CreateHabitRequestDto = CreateHabitRequestDto
  { name :: Text,
    accountId :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary CreateHabitRequestDto where
  arbitrary = do
    name <- genValidUtf8WithoutNullByte `suchThat` (not . T.null)
    accountId <- unAccountId <$> arbitrary
    pure $ CreateHabitRequestDto {..}


mkNonEmptyTextEncoder :: String -> Encoder Text Text
mkNonEmptyTextEncoder name txt = if T.null txt then Failure $ errSimple (name <> " must not be empty") else pure txt

toDomain :: Encoder CreateHabitRequestDto CreateHabitRequest
toDomain dto = do
  name <- mkNonEmptyTextEncoder "name" dto.name
  accountId <- Codec.encode accountIdFromText dto.accountId
  return $ CreateHabitRequest {..}

fromDomain :: CreateHabitResponse -> CreateHabitResponseDto
fromDomain rr =
  CreateHabitResponseDto
    { habitId = rr.habitId.unHabitId
    }

instance ToJSON CreateHabitRequestDto

instance FromJSON CreateHabitRequestDto

type CreateHabitApi = AuthProtect "JWT" :> "habits" :> ReqBody '[JSON] CreateHabitRequestDto :> Post '[JSON] CreateHabitResponseDto

createHabitRoute :: forall m. (MonadIO m, CreateHabitM m) => AuthenticatedAccount -> CreateHabitRequestDto -> ExceptT ServerError m CreateHabitResponseDto
createHabitRoute acc req = toExceptT . mapAllErrorsToServerError $
  liftE $ S.do
    req' <- fromValidation . toDomain $ req
    when (acc.accountId /= req.accountId) $ failureE UnauthorizedError
    resp <- createHabit req'
    S.pure $ fromDomain resp
