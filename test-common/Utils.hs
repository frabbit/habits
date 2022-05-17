{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Utils where

import Control.Exception
  ( Exception,
    throw,
  )
import Control.Monad.IO.Class (liftIO)

import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
    generate,
  )
import UnliftIO (MonadIO)
import Test.Hspec.Expectations.Lifted (expectationFailure, shouldBe)
import Data.Function ((&))
import Haskus.Utils.Variant.Excepts (catchLiftLeft, Excepts, type (:<), LiftVariant, Remove)

toThrow ::
  forall e es m a . (Exception e, MonadIO m, e :< es, LiftVariant (Remove e es) (Remove e es)) => Excepts es m a -> Excepts (Remove e es) m a
toThrow x = x & catchLiftLeft (\(y :: e) -> throw y)

sampleIO :: (MonadIO m, Arbitrary a) => m a
sampleIO = liftIO $ generate arbitrary

shouldBeIO :: (Show x, Eq x) => IO x -> x -> IO ()
shouldBeIO x w = do
  m <- x
  m `shouldBe` w


catchToFail :: forall e es m . (MonadIO m, e :< es, LiftVariant (Remove e es) (Remove e es)) => Excepts es m () -> Excepts (Remove e es) m ()
catchToFail c = c & catchLiftLeft  \(_::e) -> expectationFailure "No Error expected"