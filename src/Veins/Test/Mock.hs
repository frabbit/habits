{-# LANGUAGE AllowAmbiguousTypes #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Veins.Test.Mock where

import Control.Lens (set, Lens)
import qualified Control.Lens as L
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function ((&))
import Data.Kind (Type)
import Data.Type.Equality (type (==))
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Habits.Domain.AccountRepo (AccountRepo (AccountRepo))
import qualified Habits.Domain.AccountRepo as AR
import Veins.Data.HList
import qualified Veins.Data.HList as HL
import qualified Veins.Data.Type.Function as F
import qualified Veins.Data.Type.List as L
import UnliftIO (modifyTVar)
import qualified UnliftIO as U
import qualified Test.QuickCheck as QC

sampleIO :: (MonadIO m, QC.Arbitrary a) => m a
sampleIO = liftIO $ QC.generate QC.arbitrary

class MockifyArb f out | f -> out where
  mockifyArb :: f -> out

instance {-# OVERLAPPING #-} (MockifyArb b out, MkArbitraryMock a) => MockifyArb (a -> b) out where
  mockifyArb :: (a -> b) -> out
  mockifyArb f = mockifyArb (f (mkArbitraryMock @a))

instance (a ~ b) => MockifyArb a b where
  mockifyArb :: a -> b
  mockifyArb f = f

class MkArbitraryMock x where
  mkArbitraryMock :: x

instance {-# OVERLAPS #-} (MkArbitraryMock (b -> r)) => MkArbitraryMock (a -> b -> r) where
  mkArbitraryMock _ = mkArbitraryMock

instance {-# OVERLAPS #-} (MonadIO m, QC.Arbitrary r, f ~ (a -> m r)) => MkArbitraryMock f where
  mkArbitraryMock _ = sampleIO


class Mockify f out | f -> out where
  mockify :: f -> out

instance {-# OVERLAPPING #-} (Mockify b out, MkMock a) => Mockify (a -> b) out where
  mockify :: (a -> b) -> out
  mockify f = mockify (f (mkMock @a))

instance (a ~ b) => Mockify a b where
  mockify :: a -> b
  mockify f = f

class MkMock x where
  mkMock :: x

instance {-# OVERLAPS #-} (MkMock r) => MkMock (a -> r) where
  mkMock _ = mkMock

instance MkMock a where
  mkMock = error "not implemented"

class MkSpy x capture where
  mkSpy :: x -> capture -> x

instance {-# OVERLAPS #-} (MkSpy (b -> m r) (b -> m ())) => MkSpy (a -> b -> m r) (a -> b -> m ()) where
  mkSpy f capture = \a -> mkSpy (f a) (capture a)

instance (Monad m, (b -> m r) ~ f, capture ~ (b -> m ())) => MkSpy f capture where
  mkSpy f capture = \b -> do
    capture b
    f b

mapCaptureForSpy :: (F.MapReturn f (m a) (m ()) f', Monad m) => f -> f'
mapCaptureForSpy f = F.mapReturn f (\r -> r >> pure ())

data SpyContext f r = SpyContext
  { args :: [HList (F.Arguments f)],
    calls :: [(HList (F.Arguments f), r)]
  }

appendArgs :: HList (F.Arguments f) -> SpyContext f r -> SpyContext f r
appendArgs a SpyContext {..} = SpyContext {args = args <> [a], ..}

appendCall :: HList (F.Arguments f) -> r -> SpyContext f r -> SpyContext f r
appendCall a r SpyContext {..}  = SpyContext {calls = calls <> [(a, r)], ..}

emptySpyContext :: SpyContext f r
emptySpyContext = SpyContext { args = [], calls = []}

getSpyArgs :: SpyContext f r -> [HList (F.Arguments f)]
getSpyArgs SpyContext {args} = args

getSpyCalls :: SpyContext f r -> [(HList (F.Arguments f), r)]
getSpyCalls SpyContext {calls} = calls


getSpyCallsIO ::
  MonadIO m =>
  TVar (SpyContext f r) ->
  m [(HList (F.Arguments f), r)]
getSpyCallsIO = fmap getSpyCalls . U.readTVarIO

getSpyArgsIO ::
  MonadIO m =>
  TVar (SpyContext f r) ->
  m [HList (F.Arguments f)]
getSpyArgsIO = fmap getSpyArgs . U.readTVarIO

withSpy :: _ => Lens o o f f -> o -> m (TVar (SpyContext f a), o)
withSpy l o = do
  let f = L.view l o
  (var, f') <- mkSpyIO f
  pure (var, L.set l f' o)


mkSpyIO ::
  forall f f' m n a.
  ( F.ReturnOf f ~ n a,
    F.Captured f f',
    MonadIO m,
    MonadIO n,
    ( F.MapReturn
        f'
        (n a, HList (F.Arguments f))
        (n a)
        (F.ReplaceReturn f (n a))
    )
  ) =>
  f ->
  m (TVar (SpyContext f a), F.ReplaceReturn f (n a))
mkSpyIO f = do
  var <- liftIO $ newTVarIO emptySpyContext
  let map' :: forall. _ -> n a
      map' (r, args) = do
        liftIO . atomically $ modifyTVar var (appendArgs args)
        r' <- r
        liftIO . atomically $ modifyTVar var (appendCall args r')
        pure r'
      f' = F.mapReturn (F.captured f) map'
  pure (var, f')

class MockReturn x r | x -> r where
  mockReturn' :: r -> x

instance {-# OVERLAPS #-} (MockReturn (b -> r) out) => MockReturn (a -> b -> r) out where
  mockReturn' = mockReturn'

instance MockReturn (a -> r) r where
  mockReturn' r _ = r

mockReturn :: forall f r. (MockReturn f r) => r -> f -> f
mockReturn r _ = mockReturn' @f r
