{-# LANGUAGE RecordWildCards #-}
module Habits.Domain.EmailMessage where

import Habits.Prelude
import Habits.Domain.Email (Email)
import Veins.Test.QuickCheck (genValidUtf8WithoutNullByte)

newtype EmailBody = EmailBody Text deriving (Show, Eq, Ord)

instance Arbitrary EmailBody where
  arbitrary = EmailBody <$> genValidUtf8WithoutNullByte

newtype EmailSender = EmailSender Email deriving (Show, Eq, Ord)

instance Arbitrary EmailSender where
  arbitrary = EmailSender <$> arbitrary

newtype EmailReceiver = EmailReceiver Email deriving (Show, Eq, Ord)

instance Arbitrary EmailReceiver where
  arbitrary = EmailReceiver <$> arbitrary

data EmailMessage = EmailMessage {
  body :: EmailBody,
  sender :: EmailSender,
  receiver :: EmailReceiver
} deriving (Show, Eq, Ord)

instance Arbitrary EmailMessage where
  arbitrary = do
    (body, sender, receiver) <- arbitrary
    pure $ EmailMessage { .. }

