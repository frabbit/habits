{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Habits.Web.Routes.LoginRoute where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Habits.Domain.Email (Email (unEmail), emailFromText)
import Habits.Domain.Password (Password (unPassword), passwordFromText)
import qualified Habits.UseCases.Login.Class as RC
import Habits.UseCases.Login.LoginRequest (LoginRequest (..))
import Habits.UseCases.Login.LoginResponse (LoginResponse)
import Habits.Web.Utils (mapAllErrorsToServerError)
import Haskus.Utils.Variant.Excepts (liftE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Haskus.Utils.Variant.Excepts.Utils (fromValidation, toExceptT)
import Servant (ServerError)
import Servant.API (JSON, Post, ReqBody, type (:>))
import Test.QuickCheck (Arbitrary (arbitrary))
import Veins.Data.Codec (Encoder)
import qualified Veins.Data.Codec as Codec
import Veins.RecordDot.Utils (set)

data LoginResponseDto = LoginResponseDto
  { accessToken :: Text,
    refreshToken :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON LoginResponseDto

instance FromJSON LoginResponseDto

data LoginRequestDto = EmailPasswordLoginRequestDto
  { email :: Text,
    password :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary LoginRequestDto where
  arbitrary = do
    email <- unEmail <$> arbitrary
    password <- unPassword <$> arbitrary
    pure $ EmailPasswordLoginRequestDto {..}

setPassword :: _ => _
setPassword = set @"password"

setEmail :: _ => _
setEmail = set @"email"

toDomain :: Encoder LoginRequestDto LoginRequest
toDomain dto = do
  email <- Codec.encode emailFromText dto.email
  password <- Codec.encode passwordFromText dto.password
  return $ EmailPasswordLoginRequest email password

fromDomain :: LoginResponse -> LoginResponseDto
fromDomain rr =
  LoginResponseDto
    { accessToken = rr.accessToken.unAccessToken,
      refreshToken = rr.refreshToken.unRefreshToken
    }

instance ToJSON LoginRequestDto

instance FromJSON LoginRequestDto

type LoginApi = "account" :> "auth" :> ReqBody '[JSON] LoginRequestDto :> Post '[JSON] LoginResponseDto

loginRoute :: forall m. (MonadIO m, RC.Login m, _) => LoginRequestDto -> ExceptT ServerError m LoginResponseDto
loginRoute req = toExceptT . mapAllErrorsToServerError $
  liftE $ S.do
    req' <- fromValidation . toDomain $ req
    resp <- RC.login req'
    S.pure $ fromDomain resp
