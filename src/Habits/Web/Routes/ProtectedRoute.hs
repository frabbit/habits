{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Habits.Web.Routes.ProtectedRoute where

import Habits.Prelude
import Data.Aeson (FromJSON, ToJSON)
import Servant (ServerError, AuthProtect)
import Servant.API (JSON, type (:>), Get)
import Habits.Web.Auth (AuthenticatedAccount)

data ProtectedResponseDto = ProtectedResponseDto {
  accountId :: Text
}
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ProtectedResponseDto

instance FromJSON ProtectedResponseDto

type ProtectedApi = AuthProtect "JWT" :> "account" :> "auth" :> "status" :> Get '[JSON] ProtectedResponseDto

protectedRoute :: forall m. (MonadIO m) => AuthenticatedAccount -> ExceptT ServerError m ProtectedResponseDto
protectedRoute a = pure $ ProtectedResponseDto { accountId = a.accountId }




