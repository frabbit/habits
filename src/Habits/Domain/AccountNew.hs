{-# LANGUAGE TemplateHaskell #-}
module Habits.Domain.AccountNew where


import           Control.Lens                   ( makeLenses )
import           Data.Text                      ( Text )
import           Habits.Domain.Email            ( Email )
import           Test.QuickCheck                ( Arbitrary
                                                , arbitrary
                                                )

data AccountNew = AccountNew
  { _email :: Email
  , _name  :: Text
  }
  deriving (Show, Eq, Ord)

makeLenses ''AccountNew

instance Arbitrary AccountNew where
  arbitrary = do
    _email <- arbitrary
    _name  <- arbitrary
    pure $ AccountNew { _email, _name }
