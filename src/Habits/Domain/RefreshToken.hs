module Habits.Domain.RefreshToken where

import Habits.Prelude hiding (exp)
import Data.Time
  ( NominalDiffTime,
  )
import Habits.Domain.AccountId (AccountId (AccountId))
import Habits.Domain.RefreshTokenSecret (RefreshTokenSecret (RefreshTokenSecret))
import Test.QuickCheck.Instances ()
import Web.JWT (JWTClaimsSet (exp, iss, sub), claims, decodeAndVerifySignature, encodeSigned, hmacSecret, numericDate, secondsSinceEpoch, stringOrURI, toVerify)
import qualified Data.Text as Text
import Veins.Data.Codec (Codec)
import qualified Veins.Data.Codec as Codec

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

getAccountId :: RefreshTokenSecret -> RefreshToken -> Maybe AccountId
getAccountId (RefreshTokenSecret s) (RefreshToken t) = case verifiedJwt of
  Nothing -> Nothing
  Just x -> fmap (AccountId . Text.pack . show)  . sub . claims $ x
  where
    verifiedJwt = decodeAndVerifySignature (toVerify . hmacSecret $ s) t


parseRefreshToken :: Text -> Maybe RefreshToken
parseRefreshToken t
  | t == "" = Nothing
  | otherwise = Just . RefreshToken $ t

refreshTokenFromText :: Codec Text RefreshToken
refreshTokenFromText = Codec.withContext (Codec.VECNamed "RefreshTokenFromText") (Codec.Codec encoder decoder)
  where
    encoder = Codec.encoderFromMaybe (Codec.convError "Text" "RefreshToken") parseRefreshToken
    decoder p = p.unRefreshToken
