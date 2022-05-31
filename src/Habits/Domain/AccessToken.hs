module Habits.Domain.AccessToken where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time
  ( NominalDiffTime,
  )
import Habits.Domain.AccountId (AccountId (AccountId))
import Habits.Domain.AccessTokenSecret (AccessTokenSecret (AccessTokenSecret))
import Test.QuickCheck.Instances ()
import Web.JWT (JWTClaimsSet (..), claims, decodeAndVerifySignature, encodeSigned, hmacSecret, numericDate, secondsSinceEpoch, stringOrURI, toVerify, ClaimsMap (ClaimsMap))
import Prelude hiding (exp, id)
import Test.QuickCheck (Arbitrary (arbitrary))
import qualified Data.Map as Map
import Data.Aeson (object, KeyValue ((.=)))
import Debug.Trace (trace)
import qualified Data.Text as Text

newtype AccessToken = AccessToken {unAccessToken :: Text} deriving (Show, Eq, Ord)

instance Arbitrary AccessToken where
  arbitrary = do
    (secret, accountId, difftime) <- arbitrary

    let token = mkAccessToken secret accountId difftime
    pure token

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

getAccountId :: AccessTokenSecret -> AccessToken -> Maybe AccountId
getAccountId (AccessTokenSecret s) (AccessToken t) = case verifiedJwt of
  Nothing -> Nothing
  Just x -> fmap (AccountId . Text.pack . show)  . sub . claims $ x
  where
    verifiedJwt = decodeAndVerifySignature (toVerify . hmacSecret $ s) t