{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Haskus.Utils.Variant.Excepts.Syntax where

import qualified Haskus.Utils.Variant.Excepts as Exc

import Haskus.Utils.Variant.Excepts
  ( Excepts(..), runE
  )
import Haskus.Utils.Types (Union)
import Haskus.Utils.Variant.VEither
    ( pattern VLeft, pattern VRight, VEither, veitherLift, VEitherLift )
import Prelude hiding ((>>=), pure, return, (>>))
import qualified Prelude as P
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)
import GHC.TypeLits (TypeError, ErrorMessage(Text))


class (VEitherLift e1 e2) => BindLift (e1::[Type]) (e2::[Type]) where

instance (VEitherLift a b) => BindLift a b


(>>=) :: forall e1 e2 m a b . (Monad m, BindLift e1 (Union e1 e2), BindLift e2 (Union e1 e2)) => Excepts e1 m a -> (a -> Excepts e2 m b) -> Excepts (Union e1 e2) m b
(>>=) m f = Exc.Excepts $ do
  e1 <- runE m
  case e1 of
    VLeft l1 ->
      let
        v :: VEither e1 b
        v = VLeft l1
      in
      P.pure (veitherLift v :: VEither (Union e1 e2) b)
    VRight r1 -> do
      e2 <- runE $ f r1
      case e2 of
        VRight r2 -> P.pure $ VRight r2
        VLeft l2 -> P.pure $ veitherLift (VLeft l2)

pure :: (Monad m) => a -> Excepts '[] m a
pure = P.pure

return :: (Monad m) => a -> Excepts '[] m a
return = P.return

(>>) :: forall e1 e2 m a b . (Monad m, BindLift e1 (Union e1 e2), BindLift e2 (Union e1 e2)) => Excepts e1 m a -> Excepts e2 m b -> Excepts (Union e1 e2) m b
a >> b = a >>= const b

lift :: m a -> Excepts '[] m a
lift = lift

liftIO :: (MonadIO m) => IO a -> Excepts '[] m a
liftIO = liftIO



