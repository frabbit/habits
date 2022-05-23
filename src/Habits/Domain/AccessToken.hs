module Habits.Domain.AccessToken where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time
  ( NominalDiffTime,
  )
import Habits.Domain.AccountId (AccountId (AccountId))
import Habits.Domain.AccessTokenSecret (AccessTokenSecret (AccessTokenSecret))
import Test.QuickCheck.Instances ()
import Web.JWT (JWTClaimsSet (exp, iss, sub), claims, decodeAndVerifySignature, encodeSigned, hmacSecret, numericDate, secondsSinceEpoch, stringOrURI, toVerify)
import Prelude hiding (exp, id)

newtype AccessToken = AccessToken {unAccessToken :: Text} deriving (Show, Eq, Ord)

mkAccessToken :: AccessTokenSecret -> AccountId -> NominalDiffTime -> AccessToken
mkAccessToken (AccessTokenSecret s) (AccountId id) time = AccessToken jwt
  where
    cs =
      mempty
        { iss = stringOrURI ("Habits" :: Text),
          sub = stringOrURI id,
          exp = numericDate time
        }
    key = hmacSecret s
    jwt = encodeSigned key mempty cs

verifyAccessToken :: AccessTokenSecret -> AccessToken -> Bool
verifyAccessToken (AccessTokenSecret s) (AccessToken t) = isJust verifiedJwt
  where
    verifiedJwt = decodeAndVerifySignature (toVerify . hmacSecret $ s) t

isExpired :: AccessTokenSecret -> AccessToken -> NominalDiffTime -> Bool
isExpired secret token now = case getExpirationTime secret token of
  Nothing -> False
  Just d -> d < now

getExpirationTime :: AccessTokenSecret -> AccessToken -> Maybe NominalDiffTime
getExpirationTime (AccessTokenSecret s) (AccessToken t) = case verifiedJwt of
  Nothing -> Nothing
  Just x -> fmap secondsSinceEpoch . exp . claims $ x
  where
    verifiedJwt = decodeAndVerifySignature (toVerify . hmacSecret $ s) t
