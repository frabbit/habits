module Habits.TestPrelude (
  module M
) where

import Prelude as M hiding (id)

import Veins.Test.Mock as M (getSpyArgsIO, mockReturn, mockify, withSpy)
import Haskus.Utils.Variant.Excepts as M (failureE, liftE, successE, Excepts)

import Control.Monad.Reader as M (ReaderT (runReaderT))

import Control.Monad.IO.Class as M (MonadIO)

import Test.Hspec as M (Spec, fdescribe, it, describe, fit)

import Test.QuickCheck as M (property)

import Veins.Test.QuickCheck as M (propertyOne)

import Control.Monad.Except as M (runExceptT, ExceptT)