{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Habits.Web.Routes.RefreshRoute where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Habits.UseCases.Refresh.Class as RC
import Habits.UseCases.Refresh.RefreshRequest (RefreshRequest (..))
import Habits.UseCases.Refresh.RefreshResponse (RefreshResponse)
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
import Habits.Domain.RefreshToken (refreshTokenFromText, unRefreshToken)

data RefreshResponseDto = RefreshResponseDto
  { accessToken :: Text,
    refreshToken :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RefreshResponseDto

instance FromJSON RefreshResponseDto

data RefreshRequestDto = RefreshRequestDto
  { refreshToken :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary RefreshRequestDto where
  arbitrary = do
    refreshToken <- unRefreshToken <$> arbitrary

    pure $ RefreshRequestDto {..}

setRefreshToken :: _ => _
setRefreshToken = set @"refreshToken"

toDomain :: Encoder RefreshRequestDto RefreshRequest
toDomain dto = do
  token <- Codec.encode refreshTokenFromText dto.refreshToken
  return $ RefreshRequest token

fromDomain :: RefreshResponse -> RefreshResponseDto
fromDomain rr =
  RefreshResponseDto
    { accessToken = rr.accessToken.unAccessToken,
      refreshToken = rr.refreshToken.unRefreshToken
    }

instance ToJSON RefreshRequestDto

instance FromJSON RefreshRequestDto

type RefreshApi = "account" :> "refresh" :> ReqBody '[JSON] RefreshRequestDto :> Post '[JSON] RefreshResponseDto

refreshRoute :: forall m. (MonadIO m, RC.Refresh m, _) => RefreshRequestDto -> ExceptT ServerError m RefreshResponseDto
refreshRoute req = toExceptT . mapAllErrorsToServerError $
  liftE $ S.do
    req' <- fromValidation . toDomain $ req
    resp <- RC.refresh req'
    S.pure $ fromDomain resp
