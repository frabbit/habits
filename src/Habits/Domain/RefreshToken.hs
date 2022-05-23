module Habits.Domain.RefreshToken where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time
  ( NominalDiffTime,
  )
import Habits.Domain.AccountId (AccountId (AccountId))
import Habits.Domain.RefreshTokenSecret (RefreshTokenSecret (RefreshTokenSecret))
import Test.QuickCheck.Instances ()
import Web.JWT (JWTClaimsSet (exp, iss, sub), claims, decodeAndVerifySignature, encodeSigned, hmacSecret, numericDate, secondsSinceEpoch, stringOrURI, toVerify)
import Prelude hiding (exp, id)
import Test.QuickCheck (Arbitrary (arbitrary))

newtype RefreshToken = RefreshToken {unRefreshToken :: Text} deriving (Show, Eq, Ord)

instance Arbitrary RefreshToken where
  arbitrary = do
    (secret, accountId, difftime) <- arbitrary

    let token = mkRefreshToken secret accountId difftime
    pure token

mkRefreshToken :: RefreshTokenSecret -> AccountId -> NominalDiffTime -> RefreshToken
mkRefreshToken (RefreshTokenSecret s) (AccountId id) time = RefreshToken jwt
  where
    cs =
      mempty
        { iss = stringOrURI ("Habits" :: Text),
          sub = stringOrURI id,
          exp = numericDate time
        }
    key = hmacSecret s
    jwt = encodeSigned key mempty cs

verifyRefreshToken :: RefreshTokenSecret -> RefreshToken -> Bool
verifyRefreshToken (RefreshTokenSecret s) (RefreshToken t) = isJust verifiedJwt
  where
    verifiedJwt = decodeAndVerifySignature (toVerify . hmacSecret $ s) t

isExpired :: RefreshTokenSecret -> RefreshToken -> NominalDiffTime -> Bool
isExpired secret token now = case getExpirationTime secret token of
  Nothing -> False
  Just d -> d < now

getExpirationTime :: RefreshTokenSecret -> RefreshToken -> Maybe NominalDiffTime
getExpirationTime (RefreshTokenSecret s) (RefreshToken t) = case verifiedJwt of
  Nothing -> Nothing
  Just x -> fmap secondsSinceEpoch . exp . claims $ x
  where
    verifiedJwt = decodeAndVerifySignature (toVerify . hmacSecret $ s) t
