module Habits.Domain.PasswordHash where

import Habits.Prelude
import qualified Data.Password.Bcrypt as PW
import Test.QuickCheck.Instances ()
import Habits.Domain.Password (Password(..))
import qualified Data.ByteString as BS
import Test.QuickCheck (vector)

newtype PasswordHash = PasswordHash {unPasswordHash :: Text} deriving (Show, Eq, Ord)

mkFromPassword :: (MonadIO m) => Password -> m PasswordHash
mkFromPassword (Password pw) = do
  hashed <- PW.hashPassword $ PW.mkPassword pw
  pure . PasswordHash . PW.unPasswordHash $ hashed

isValid :: Password -> PasswordHash -> Bool
isValid (Password pw) (PasswordHash ph) = PW.checkPassword (PW.mkPassword pw) (PW.PasswordHash ph) == PW.PasswordCheckSuccess

mkFromPasswordWithSalt :: PW.Salt PW.Bcrypt -> Password -> PasswordHash
mkFromPasswordWithSalt salt (Password pw) = PasswordHash . PW.unPasswordHash $ hashed
  where
    cost = 4 -- between 4 and 31
    hashed = PW.hashPasswordWithSalt cost salt (PW.mkPassword pw)

genSalt :: Gen (PW.Salt a)
genSalt = PW.Salt . BS.pack <$> vector 16

instance Arbitrary PasswordHash where
  arbitrary = do
    salt <- genSalt
    mkFromPasswordWithSalt salt <$> arbitrary