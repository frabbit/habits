module Habits.Domain.RefreshTokenHash
  ( RefreshTokenHash (..),
  mkFromRefreshToken, isValid)
where

import Prelude
import Data.Text (Text)
import Test.QuickCheck.Instances ()
import Test.QuickCheck (Arbitrary (arbitrary), Gen, vector)
import qualified Data.Password.Bcrypt as PW
import Habits.Domain.RefreshToken (RefreshToken (RefreshToken))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as BS

newtype RefreshTokenHash = RefreshTokenHash {unRefreshTokenHash :: Text} deriving (Show, Eq, Ord)

mkFromRefreshTokenWithSalt :: PW.Salt PW.Bcrypt -> RefreshToken -> RefreshTokenHash
mkFromRefreshTokenWithSalt salt (RefreshToken pw) = RefreshTokenHash . PW.unPasswordHash $ hashed
  where
    hashed = PW.hashPasswordWithSalt 4 salt (PW.mkPassword pw)

mkFromRefreshToken :: (MonadIO m) => RefreshToken -> m RefreshTokenHash
mkFromRefreshToken (RefreshToken token) = do
  hashed <- PW.hashPassword $ PW.mkPassword token
  pure . RefreshTokenHash . PW.unPasswordHash $ hashed

isValid :: RefreshToken -> RefreshTokenHash -> Bool
isValid (RefreshToken token) (RefreshTokenHash hash) = PW.checkPassword (PW.mkPassword token) (PW.PasswordHash hash) == PW.PasswordCheckSuccess

genSalt :: Gen (PW.Salt a)
genSalt = PW.Salt . BS.pack <$> vector 16

instance Arbitrary RefreshTokenHash where
  arbitrary = do
    salt <- genSalt
    mkFromRefreshTokenWithSalt salt <$> arbitrary