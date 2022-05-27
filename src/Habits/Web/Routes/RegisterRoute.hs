{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Habits.Web.Routes.RegisterRoute where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Habits.Domain.Email (Email (unEmail), emailFromText)
import Habits.Domain.Password (Password (unPassword), passwordFromText)
import qualified Habits.UseCases.Register.Class as RC
import Habits.UseCases.Register.RegisterRequest (RegisterRequest (..))
import Habits.UseCases.Register.RegisterResponse (RegisterResponse)
import Habits.Web.Utils (mapAllErrorsToServerError)
import Haskus.Utils.Variant.Excepts (liftE)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Haskus.Utils.Variant.Excepts.Utils (fromValidation, toExceptT)
import Servant (ServerError)
import Servant.API (JSON, Post, ReqBody, type (:>))
import Test.QuickCheck (Arbitrary (arbitrary))
import Veins.Data.Codec (Encoder, idCodec)
import qualified Veins.Data.Codec as Codec
import Veins.RecordDot.Utils (set)
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)

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

registerRoute :: forall m. (MonadIO m, RC.Register m, _) => RegisterRequestDto -> ExceptT ServerError m RegisterResponseDto
registerRoute req = toExceptT . mapAllErrorsToServerError . liftE $ S.do
  req' <- fromValidation . toDomain $ req
  resp <- RC.execute req'
  S.pure $ fromDomain resp
