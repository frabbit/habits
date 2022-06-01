{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Haskus.Utils.Variant.Excepts.Utils where

import Prelude

import Control.Monad.Except (ExceptT (ExceptT))
import Haskus.Utils.Variant.Excepts (Excepts, successE, evalE, catchLiftLeft, Remove, catchLiftBoth, failureE)
import Data.Function ((&))
import Haskus.Utils.Types (Union)
import Data.Validation (Validation (..))

toExceptT :: forall e m a . (Monad m) => Excepts '[e] m a -> ExceptT e m a
toExceptT e =
  e & fmap Right & catchLiftLeft (\(e1::e) -> successE . Left $ e1) & evalE & ExceptT

catchExcepts :: forall e m a e1 es . (Monad m, _) => (e -> Excepts e1 m a) -> Excepts es m a -> Excepts (Union e1 (Remove e es)) m a
catchExcepts = catchLiftBoth

fromValidation :: (Monad m) => Validation e a -> Excepts '[e] m a
fromValidation = \case
  Success r -> pure r
  Failure f -> failureE f