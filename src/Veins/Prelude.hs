module Veins.Prelude (
  module M,
  identity
) where

import Prelude as M hiding (id)

import qualified Prelude

import Data.Function as M ((&))
import Data.Functor as M ((<&>))

import Control.Monad as M (replicateM, when, unless)

import Data.Proxy as M (Proxy (Proxy))

import GHC.Generics as M (Generic)

import Haskus.Utils.Variant.Excepts as M (failureE, liftE, successE, Excepts, evalE, throwE)

import Control.Monad.Reader as M (ReaderT (runReaderT), MonadReader, asks)

import Control.Monad.IO.Class as M (MonadIO, liftIO)

import Test.QuickCheck as M (Arbitrary, arbitrary)

import Control.Monad.Except as M (runExceptT, ExceptT(ExceptT), MonadError (throwError))

import Data.Maybe as M (isJust)

import Data.Either as M (isRight, isLeft)

import Data.Text as M (Text)

identity :: a -> a
identity = Prelude.id
{-# INLINE identity #-}