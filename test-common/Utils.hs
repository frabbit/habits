{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
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
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Variant
  ( Catch,
    Variant,
    catchM, CatchF,
  )
import GHC.Base (coerce)
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
    generate,
  )
import UnliftIO (MonadIO)
import Data.Kind (Type)
import Test.Hspec.Expectations.Lifted (expectationFailure, shouldBe)

toThrow ::
  forall x e m a e'.
  (Catch x e e', MonadIO m, Exception x) =>
  ExceptT (Variant e) m a ->
  ExceptT (Variant e') m a
toThrow x = x `catchM` (\(y :: x) -> throw y)

sampleIO :: (MonadIO m, Arbitrary a) => m a
sampleIO = liftIO $ generate arbitrary

shouldBeIO :: (Show x, Eq x) => IO x -> x -> IO ()
shouldBeIO x w = do
  m <- x
  m `shouldBe` w

class ToThrowAll e e1 | e -> e1 where
  toThrowAll :: (MonadIO m) => ExceptT (Variant e) m a -> ExceptT (Variant e1) m a

instance (e ~ '[]) => ToThrowAll e '[] where
  toThrowAll ::
    forall m a. ExceptT (Variant '[]) m a -> ExceptT (Variant '[]) m a
  toThrowAll = coerce

instance {-# OVERLAPS #-} (ToThrowAll e' '[], Catch e (e ': e') e', Exception e) => ToThrowAll (e ': e') '[] where
  toThrowAll ::
    forall m a.
    (MonadIO m) =>
    ExceptT (Variant (e ': e')) m a ->
    ExceptT (Variant '[]) m a
  toThrowAll x = toThrowAll (toThrow @e x :: ExceptT (Variant e') m a)


catchToFail :: forall x (e::[Type]) e1 m . (MonadIO m, CatchF x e e1) => ExceptT (Variant e) m () -> ExceptT (Variant e1) m ()
catchToFail c = c `catchM` \(_::x) -> expectationFailure "No Error expected"