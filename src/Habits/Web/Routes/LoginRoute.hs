{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Habits.Web.Routes.LoginRoute where

import Habits.Prelude
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Habits.Domain.Email (Email (unEmail), emailFromText)
import Habits.Domain.Password (Password (unPassword), passwordFromText)
import Habits.UseCases.Login.LoginRequest (LoginRequest (..))
import Habits.UseCases.Login.LoginResponse (LoginResponse)
import Habits.Web.Utils (mapAllErrorsToServerError)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Haskus.Utils.Variant.Excepts.Utils (fromValidation, toExceptT)
import Servant (ServerError)
import Servant.API (JSON, Post, ReqBody, type (:>))
import Veins.Data.Codec (Encoder)
import qualified Veins.Data.Codec as Codec
import Veins.RecordDot.Utils (set)
import Habits.UseCases.Login.Class (LoginM (login))

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

loginRoute :: forall m. (MonadIO m, LoginM m, _) => LoginRequestDto -> ExceptT ServerError m LoginResponseDto
loginRoute req = toExceptT . mapAllErrorsToServerError $
  liftE $ S.do
    req' <- fromValidation . toDomain $ req
    resp <- login req'
    S.pure $ fromDomain resp
