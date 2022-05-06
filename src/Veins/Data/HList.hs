{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Veins.Data.HList
  ( hnil,
    hcons,
    (#:),
    HList (..),
    HRemove (..),
    HConcat (..),
    HGetFirst (..),
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Veins.Data.Type.List as L

data HList (x :: [Type]) where
  HCons :: forall a tail. a -> HList tail -> HList (a ': tail)
  HNil :: HList '[]

(#:) :: a -> HList tail -> HList (a : tail)
(#:) = HCons

infixr 5 #:

hnil :: HList '[]
hnil = HNil

hcons ::
  forall x (tail :: [Type]).
  x ->
  HList tail ->
  HList (x ': tail)
hcons = HCons

instance Show (HList '[]) where
  show _ = "[]"

instance forall x (xs :: [Type]). (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
  show (HCons x xs) = "" <> show x <> " #: " <> show xs <> ""

instance Eq (HList '[]) where
  (==) _ _ = True

instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs)) where
  (==) (HCons a as) (HCons b bs) = a == b && as == bs

class HRemove x (xs :: [Type]) r | x xs -> r where
  hremove :: Proxy x -> HList xs -> HList r

instance HRemove x '[] '[] where
  hremove _ _ = HNil

instance {-# OVERLAPS #-} HRemove x (x ': xs) xs where
  hremove _ (HCons _ t) = t

instance (HRemove x xs z) => HRemove x (a ': xs) z where
  hremove p (HCons _ t) = hremove p t

class HConcat (xs :: [Type]) (ys :: [Type]) (r :: [Type]) | xs ys -> r where
  hconcat :: HList xs -> HList ys -> HList r

instance HConcat '[] ys ys where
  hconcat _ b = b

instance (HConcat xs ys z) => HConcat (x ': xs) ys (x ': z) where
  hconcat (HCons x xs) ys = hcons x (hconcat xs ys)

class HGetFirst x xs where
  hgetFirst :: HList xs -> x

instance HGetFirst x (x ': xs) where
  hgetFirst (HCons x _) = x

instance {-# OVERLAPS #-} (HGetFirst x xs) => HGetFirst x (y ': xs) where
  hgetFirst (HCons _ xs) = hgetFirst xs
