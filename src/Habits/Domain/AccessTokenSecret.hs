module Habits.Domain.AccessTokenSecret
  ( mkAccessTokenSecret,
    AccessTokenSecret(..),
  )
where

import Data.Text (Text)
import Test.QuickCheck.Instances ()
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements)
import Control.Monad (replicateM)
import qualified Data.Text as Text

newtype AccessTokenSecret = AccessTokenSecret {unAccessTokenSecret :: Text} deriving (Show, Eq, Ord)

mkAccessTokenSecret :: Text -> AccessTokenSecret
mkAccessTokenSecret = AccessTokenSecret


genChar :: Gen Char
genChar = elements (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "~!@#*+-()[]^$")

instance Arbitrary AccessTokenSecret where
  arbitrary = do
    secret <- replicateM 32 genChar
    pure . AccessTokenSecret $ Text.pack secret