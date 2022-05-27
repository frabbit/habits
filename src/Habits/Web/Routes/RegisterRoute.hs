{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Habits.Web.Routes.RegisterRoute where

import Control.Monad.Except (ExceptT (ExceptT))
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Servant.API (type (:>), Post, JSON, ReqBody)
import Servant (Server, Handler (Handler), Proxy (Proxy), serve, ServerT, hoistServer, ServerError, err500, err400)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Habits.UseCases.Register.Class as RC
import Haskus.Utils.Variant.Excepts (Excepts, catchLiftRight, successE, evalE, catchLiftLeft, Remove, failureE, catchLiftBoth)
import Data.Function ((&))
import Habits.UseCases.Register.RegisterResponse (RegisterResponse)
import Habits.Domain.RepositoryError (RepositoryError(RepositoryError))
import Habits.Domain.EmailAlreadyUsedError (EmailAlreadyUsedError(EmailAlreadyUsedError))
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Haskus.Utils.Types.List (Union)
import Veins.Test.QuickCheck (sampleIO, genValidUtf8WithoutNullByte)
import Control.Monad.IO.Class (MonadIO)
import Haskus.Utils.Variant.Excepts.Utils (toExceptT, catchExcepts, fromValidation)
import Habits.Domain.Email (Email(unEmail), emailFromText)
import Test.QuickCheck (Arbitrary (arbitrary), Gen)
import Habits.Domain.Password (Password(unPassword), passwordFromText)
import qualified Veins.Data.Codec as Codec
import Veins.Data.Codec (Encoder, idCodec)
import Habits.UseCases.Register (RegisterRequest (..))
import Data.Validation (Validation(..))
import Veins.RecordDot.Utils (set)

newtype RegisterResponseDto = RegisterResponseDto
  { accountId :: Text
  }  deriving (Show, Eq, Ord, Generic)

instance ToJSON RegisterResponseDto
instance FromJSON RegisterResponseDto

data RegisterRequestDto = RegisterRequestDto
  { email :: Text,
    name :: Text,
    password :: Text
  } deriving (Show, Eq, Ord, Generic)

instance Arbitrary RegisterRequestDto where
  arbitrary = do
    email <- unEmail <$> arbitrary
    name <- genValidUtf8WithoutNullByte
    password <- unPassword <$> arbitrary
    pure $ RegisterRequestDto { .. }

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
  return $ RegisterRequest { .. }


fromDomain :: RegisterResponse -> RegisterResponseDto
fromDomain rr = RegisterResponseDto {
  accountId = rr.accountId.unAccountId
}

instance ToJSON RegisterRequestDto
instance FromJSON RegisterRequestDto

type RegisterApi = "account" :> ReqBody '[JSON] RegisterRequestDto :> Post '[JSON] RegisterResponseDto

registerRoute :: forall m . (MonadIO m, RC.Register m, _) => RegisterRequestDto -> ExceptT ServerError m RegisterResponseDto
registerRoute req = toExceptT x
  where
    x = S.do
      req' <- fromValidation . toDomain $ req
      resp <- RC.execute req'
      S.pure $ fromDomain resp
      & catchExcepts (\(_ :: Codec.ValidationError) -> failureE err400)
      & catchExcepts (\(_ :: EmailAlreadyUsedError) -> failureE err500)
      & catchExcepts (\(_ :: RepositoryError) -> failureE err500)



--server :: (Register m) => ServerT RegisterApi (ExceptT ServerError m)
--server = register

--registerApi :: Proxy RegisterApi
--registerApi = Proxy

--nt :: ExceptT ServerError IO a -> Handler a
--nt = Handler
--
--app :: Application
--app = serve registerApi $ hoistServer registerApi nt server
--
--main :: IO ()
--main = run 8081 app