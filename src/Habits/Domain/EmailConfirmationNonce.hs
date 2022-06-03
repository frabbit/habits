module Habits.Domain.EmailConfirmationNonce where

import Habits.Prelude

import Test.QuickCheck.Instances.UUID ()
import Veins.Crypto.Nonce.Utils (arbitraryNonce)
import Crypto.Nonce (nonce128urlT, withGenerator)
import UnliftIO (MonadUnliftIO)

newtype EmailConfirmationNonce = EmailConfirmationNonce { unEmailConfirmationNonce :: Text } deriving (Eq, Show, Ord)

instance Arbitrary EmailConfirmationNonce where
  arbitrary = EmailConfirmationNonce <$> arbitraryNonce

newEmailConfirmationNonce :: MonadUnliftIO m => m EmailConfirmationNonce
newEmailConfirmationNonce = withGenerator (fmap EmailConfirmationNonce . nonce128urlT )
