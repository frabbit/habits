{-# LANGUAGE DeriveGeneric #-}
module Habits.Web.Auth where

import GHC.Generics (Generic)
import Servant (AuthProtect)
import Data.Text (Text, splitOn)
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler)
import qualified Habits.Domain.AccessToken as AT
import Habits.Domain.AccessTokenSecret (AccessTokenSecret(..))
import qualified Data.Text.Encoding as Text
import Data.Either.Extra (eitherToMaybe, maybeToEither)
import Control.Monad (unless, when)
import qualified Data.Time.Clock.POSIX as Posix
import Network.Wai (requestHeaders)
import qualified Network.Wai as Wai
import Data.Time (UTCTime)
import Veins.Data.List.Utils (safeHead)


data AuthenticatedAccount = AuthenticatedAccount {
  accountId :: Text
} deriving (Generic, Eq, Show)

type instance AuthServerData (AuthProtect "JWT") = AuthenticatedAccount

type JWTAuthHandler = AuthHandler Wai.Request AuthenticatedAccount

data TokenValidationError
  = TveExpired
  | TveInvalidHeader
  | TveUnverified
  | TveNoAccountId

parseAuthenticatedAccount :: UTCTime -> AccessTokenSecret -> Wai.Request -> Either TokenValidationError AuthenticatedAccount
parseAuthenticatedAccount timeNow secret req = do
  token <- maybeToEither TveInvalidHeader $ parseTokenFromHeader =<< getAuthorizationHeader req
  unless (AT.verifyAccessToken secret (AT.AccessToken token)) $ Left TveUnverified
  accountId <- maybeToEither TveNoAccountId $ AT.getAccountId secret (AT.AccessToken token)
  when (AT.isExpired secret (AT.AccessToken token) (Posix.utcTimeToPOSIXSeconds timeNow)) $ Left TveExpired
  Right $ AuthenticatedAccount {accountId = accountId.unAccountId}
  where
    getAuthorizationHeader :: Wai.Request -> Maybe Text
    getAuthorizationHeader r = eitherToMaybe . Text.decodeUtf8' =<< lookup "Authorization" (requestHeaders r)
    parseTokenFromHeader :: Text -> Maybe Text
    parseTokenFromHeader = safeHead . drop 1 . splitOn " "