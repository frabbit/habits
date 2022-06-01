module Veins.Prelude (
  module M,
  identity
) where

import Prelude as M hiding (id)

import qualified Prelude

import Data.Function as M ((&))
import Data.Functor as M ((<&>))

import Haskus.Utils.Variant.Excepts as M (failureE, liftE, successE, Excepts, evalE)

import Control.Monad.Reader as M (ReaderT (runReaderT))

import Control.Monad.IO.Class as M (MonadIO)

import Test.QuickCheck as M (Arbitrary, arbitrary)

import Control.Monad.Except as M (runExceptT, ExceptT)

import Data.Maybe as M (isJust)

import Data.Either as M (isRight, isLeft)

identity :: a -> a
identity = Prelude.id
{-# INLINE identity #-}