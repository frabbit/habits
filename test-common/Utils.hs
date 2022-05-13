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
import Data.Function ((&))
import Haskus.Utils.Variant.Excepts (catchLiftLeft, Excepts, type (:<), LiftVariant, Remove)

toThrow ::
  forall e es es' m a . (Exception e, MonadIO m, e :< es, LiftVariant (Remove e es) (Remove e es)) => Excepts es m a -> Excepts (Remove e es) m a
toThrow x = x & catchLiftLeft (\(y :: e) -> throw y)

sampleIO :: (MonadIO m, Arbitrary a) => m a
sampleIO = liftIO $ generate arbitrary

shouldBeIO :: (Show x, Eq x) => IO x -> x -> IO ()
shouldBeIO x w = do
  m <- x
  m `shouldBe` w



--catchToFail :: forall x (e::[Type]) e1 m . (MonadIO m, CatchF x e e1) => ExceptT (Variant e) m () -> ExceptT (Variant e1) m ()

catchToFail :: forall e es m . (MonadIO m, e :< es, LiftVariant (Remove e es) (Remove e es)) => Excepts es m () -> Excepts (Remove e es) m ()
catchToFail c = c & catchLiftLeft  \(_::e) -> expectationFailure "No Error expected"