{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Haskus.Utils.Variant.Excepts.Syntax where

import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as IOC
import qualified Control.Monad.Trans.Class as T
import Data.Kind (Type)
import Haskus.Utils.Types (Union)
import Haskus.Utils.Variant.Excepts
  ( Excepts (..),
    runE, liftE,
  )
import qualified Haskus.Utils.Variant.Excepts as Exc
import Haskus.Utils.Variant.VEither
  ( VEither,
    VEitherLift,
    veitherLift,
    pattern VLeft,
    pattern VRight,
  )
import Prelude hiding (pure, return, (>>), (>>=))
import qualified Prelude as P

class (VEitherLift e1 e2) => BindLift (e1 :: [Type]) (e2 :: [Type])

instance (VEitherLift a b) => BindLift a b

coerce :: Excepts '[] m a -> Excepts '[] m a
coerce = id

fail :: P.MonadFail m => P.String -> Excepts es m a
fail = P.fail

pure :: (Monad m) => a -> Excepts '[] m a
pure = P.pure

return :: (Monad m) => a -> Excepts '[] m a
return = P.return

fmap :: _ => (a -> b) -> Excepts e1 m a -> Excepts e1 m b
fmap = P.fmap

(<*>) :: _ => Excepts e1 m (a -> b) -> Excepts e2 m a -> Excepts (Union e1 e2) m b
(<*>) f a = liftE f P.<*> liftE a

(>>=) :: forall e1 e2 m a b. (Monad m, BindLift e1 (Union e1 e2), BindLift e2 (Union e1 e2)) => Excepts e1 m a -> (a -> Excepts e2 m b) -> Excepts (Union e1 e2) m b
(>>=) m f = Exc.Excepts $ do
  e1 <- runE m
  case e1 of
    VLeft l1 ->
      let v :: VEither e1 b
          v = VLeft l1
       in P.pure (veitherLift v :: VEither (Union e1 e2) b)
    VRight r1 -> do
      e2 <- runE $ f r1
      case e2 of
        VRight r2 -> P.pure $ VRight r2
        VLeft l2 -> P.pure $ veitherLift (VLeft l2)

(>>) :: forall e1 e2 m a b . (Monad m, BindLift e1 (Union e1 e2), BindLift e2 (Union e1 e2)) => Excepts e1 m a -> Excepts e2 m b -> Excepts (Union e1 e2) m b
a >> b = a >>= const b

liftWithErrors :: (Monad m) => m a -> Excepts err m a
liftWithErrors = T.lift

lift :: (Monad m) => m a -> Excepts '[] m a
lift = T.lift

liftIO :: (MonadIO m) => IO a -> Excepts '[] m a
liftIO = IOC.liftIO
