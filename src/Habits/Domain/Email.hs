module Habits.Domain.Email where
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.QuickCheck.Instances
import           Control.Monad                  ( liftM )
import           Data.Text                      ( Text )
import           Test.QuickCheck                ( arbitrary, Arbitrary )
newtype Email = Email Text deriving (Show, Eq, Ord)

instance Arbitrary Email where
  arbitrary = do
    Email <$> arbitrary
