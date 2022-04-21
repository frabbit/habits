{-# LANGUAGE NamedFieldPuns #-}

module Habits.Domain.AccountNew where


import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Habits.Domain.Email            ( Email )
import           Test.QuickCheck                ( Arbitrary
                                                , arbitrary
                                                )

data AccountNew = AccountNew
  { email :: Email
  , name  :: Text
  }
  deriving (Show, Eq, Ord)

instance Arbitrary AccountNew where
  arbitrary = do
    email <- arbitrary
    name  <- arbitrary
    pure $ AccountNew { email, name }
