module Habits.Domain.RefreshTokenSecret
  ( mkRefreshTokenSecret,
    RefreshTokenSecret(..),
  )
where

import Habits.Prelude
import Test.QuickCheck.Instances ()
import Test.QuickCheck (elements)
import qualified Data.Text as Text

newtype RefreshTokenSecret = RefreshTokenSecret {unRefreshTokenSecret :: Text} deriving (Show, Eq, Ord)

mkRefreshTokenSecret :: Text -> RefreshTokenSecret
mkRefreshTokenSecret = RefreshTokenSecret


genChar :: Gen Char
genChar = elements (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "~!@#*+-()[]^$")

instance Arbitrary RefreshTokenSecret where
  arbitrary = do
    secret <- replicateM 32 genChar
    pure . RefreshTokenSecret $ Text.pack secret