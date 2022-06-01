module Veins.Test.Prelude (
  module M,
  identity
) where

import Veins.Test.HSpec.TH as M (shouldMatchPattern)

import Prelude as M hiding (id)

import qualified Prelude

import Data.Function as M ((&))
import Data.Functor as M ((<&>))

import Veins.Test.Mock as M (getSpyArgsIO, mockReturn, mockify, withSpy)
import Haskus.Utils.Variant.Excepts as M (failureE, liftE, successE, Excepts, evalE)

import Control.Monad.Reader as M (ReaderT (runReaderT))

import Control.Monad.IO.Class as M (MonadIO)

import Test.Hspec as M (Spec, fdescribe, it, describe, fit)

import Test.QuickCheck as M (property)

import Veins.Test.QuickCheck as M (propertyOne)

import Control.Monad.Except as M (runExceptT, ExceptT)

import Data.Maybe as M (isJust)

import Data.Either as M (isRight, isLeft)

identity :: a -> a
identity = Prelude.id
{-# INLINE identity #-}