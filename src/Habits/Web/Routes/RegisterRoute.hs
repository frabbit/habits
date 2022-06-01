{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Habits.Web.Routes.RegisterRoute where

import Habits.Prelude

import Data.Aeson (FromJSON, ToJSON)
import Habits.Domain.Email (Email (unEmail), emailFromText)
import Habits.Domain.Password (Password (unPassword), passwordFromText)

import Habits.UseCases.Register.RegisterRequest (RegisterRequest (..))
import Habits.UseCases.Register.RegisterResponse (RegisterResponse)
import Habits.Web.Utils (mapAllErrorsToServerError)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Haskus.Utils.Variant.Excepts.Utils (fromValidation, toExceptT)
import Servant (ServerError)
import Servant.API (JSON, Post, ReqBody, type (:>))
import Veins.Data.Codec (Encoder, idCodec)
import qualified Veins.Data.Codec as Codec
import Veins.RecordDot.Utils (set)
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)
import Habits.UseCases.Register.Class (RegisterM (register))

newtype RegisterResponseDto = RegisterResponseDto
  { accountId :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RegisterResponseDto

instance FromJSON RegisterResponseDto

data RegisterRequestDto = RegisterRequestDto
  { email :: Text,
    name :: Text,
    password :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary RegisterRequestDto where
  arbitrary = do
    email <- unEmail <$> arbitrary
    name <- genValidUtf8WithoutNullByte
    password <- unPassword <$> arbitrary
    pure $ RegisterRequestDto {..}

setPassword :: _ => _
setPassword = set @"password"

setName :: _ => _
setName = set @"name"

setEmail :: _ => _
setEmail = set @"email"

toDomain :: Encoder RegisterRequestDto RegisterRequest
toDomain dto = do
  email <- Codec.encode emailFromText dto.email
  name <- Codec.encode idCodec dto.name
  password <- Codec.encode passwordFromText dto.password
  return $ RegisterRequest {..}

fromDomain :: RegisterResponse -> RegisterResponseDto
fromDomain rr =
  RegisterResponseDto
    { accountId = rr.accountId.unAccountId
    }

instance ToJSON RegisterRequestDto

instance FromJSON RegisterRequestDto

type RegisterApi = "account" :> ReqBody '[JSON] RegisterRequestDto :> Post '[JSON] RegisterResponseDto

registerRoute :: forall m. (MonadIO m, RegisterM m, _) => RegisterRequestDto -> ExceptT ServerError m RegisterResponseDto
registerRoute req = toExceptT . mapAllErrorsToServerError . liftE $ S.do
  req' <- fromValidation . toDomain $ req
  resp <- register req'
  S.pure $ fromDomain resp
