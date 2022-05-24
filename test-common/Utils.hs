{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

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
import Haskus.Utils.Variant.Excepts (catchLiftLeft, Excepts, type (:<), LiftVariant, Remove, V, catchAllE)
import Data.Typeable (Typeable, typeRep, Proxy (..))

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

expectError :: forall e es m a . (Typeable e, MonadIO m, e :< es, LiftVariant (Remove e es) (Remove e es)) => Excepts es m a -> Excepts (Remove e es) m ()
expectError c = do
  c
  expectationFailure $ "Got no error, but expected " <> show (typeRep (Proxy::Proxy e))
  & catchLiftLeft @e (const $ pure ())

catchAllToFail :: forall es m . (MonadIO m, Show (V es)) => Excepts es m () -> Excepts '[] m ()
catchAllToFail c = c & catchAllE \(x::V es) -> expectationFailure $ "No Error expected, but " <> show x <> " received"