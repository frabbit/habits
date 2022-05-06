{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Utils where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.Except           ( ExceptT )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Variant                   ( Catch
                                                , Variant
                                                , catchM
                                                )
import           GHC.Base                       ( coerce )
import           Test.QuickCheck                ( Arbitrary
                                                , arbitrary
                                                , generate
                                                )
import           UnliftIO                       ( MonadIO )


toThrow
  :: forall x e m a e'
   . (Catch x e e', MonadIO m, Exception x)
  => ExceptT (Variant e) m a
  -> ExceptT (Variant e') m a
toThrow x = x `catchM` (\(y :: x) -> throw y)

sampleIO :: (MonadIO m, Arbitrary a) => m a
sampleIO = liftIO $ generate arbitrary


class ToThrowAll e e1 | e -> e1 where
  toThrowAll :: (MonadIO m) => ExceptT (Variant e) m a -> ExceptT (Variant e1) m a

instance (e ~ '[]) => ToThrowAll e '[] where
  toThrowAll
    :: forall m a . ExceptT (Variant '[]) m a -> ExceptT (Variant '[]) m a
  toThrowAll = coerce

instance {-# OVERLAPS #-} (ToThrowAll e' '[], Catch e (e ': e') e', Exception e) => ToThrowAll (e ': e') '[] where
  toThrowAll
    :: forall m a
     . (MonadIO m)
    => ExceptT (Variant (e ': e')) m a
    -> ExceptT (Variant '[]) m a
  toThrowAll x = toThrowAll (toThrow @e x :: ExceptT (Variant e') m a)
