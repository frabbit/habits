module Habits.TestUtils where

import Habits.Test.Prelude
import Habits.Domain.AccountNew (AccountNew)

newtype AccountNewConfirmed = AccountNewConfirmed AccountNew
instance Arbitrary AccountNewConfirmed where arbitrary = AccountNewConfirmed <$> (arbitrary <&> (\a -> a{emailConfirmed = True}))

newtype AccountNewUnconfirmed = AccountNewUnconfirmed AccountNew
instance Arbitrary AccountNewUnconfirmed where arbitrary = AccountNewUnconfirmed <$> (arbitrary <&> (\a -> a{emailConfirmed = False}))