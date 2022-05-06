{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Habits.Domain.AccountNew where


import           Control.Lens                   ( makeLenses )
import           Data.Text                      ( Text )
import           Habits.Domain.Email            ( Email )
import           Habits.Domain.Password         ( Password )
import           Test.QuickCheck                ( Arbitrary
                                                , arbitrary
                                                )

data AccountNew = AccountNew
  { _email    :: Email
  , _name     :: Text
  , _password :: Password
  }
  deriving (Show, Eq, Ord)

makeLenses ''AccountNew

instance Arbitrary AccountNew where
  arbitrary = do
    _email    <- arbitrary
    _name     <- arbitrary
    _password <- arbitrary
    pure $ AccountNew { .. }
