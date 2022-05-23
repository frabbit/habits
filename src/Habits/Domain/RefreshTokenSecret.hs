module Habits.Domain.RefreshTokenSecret
  ( mkRefreshTokenSecret,
    RefreshTokenSecret(..),
  )
where

import Data.Text (Text)
import Test.QuickCheck.Instances ()
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements)
import Control.Monad (replicateM)
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