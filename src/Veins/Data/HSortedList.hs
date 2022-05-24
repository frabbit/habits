{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Veins.Data.HSortedList where

import Data.Function ((&))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Type.Equality (type (==))
import qualified Veins.Data.HList as L
import Veins.Data.ToSymbol (CmpToSymbol)

newtype HSortedList (xs :: [Type]) = HSortedList (L.HList xs)

instance (Show (L.HList x)) => Show (HSortedList x) where
  show = show . unwrap

instance (Eq (L.HList x)) => Eq (HSortedList x) where
  a == b = unwrap a == unwrap b

empty :: HSortedList '[]
empty = HSortedList L.hnil

unwrap :: HSortedList xs -> L.HList xs
unwrap (HSortedList l) = l

type family HandleOrd (x :: Ordering) onLt onEq onGt :: k where
  HandleOrd 'LT x _ _ = x
  HandleOrd 'EQ _ x _ = x
  HandleOrd 'GT _ _ x = x

type Insert :: forall k. k -> [k] -> [k]
type family Insert x xs where
  Insert x '[] = '[x]
  Insert x (y ': xs) = HandleOrd (CmpToSymbol x y) (x ': y ': xs) (x ': y ': xs) (y ': Insert x xs)

class DemoteOrdering (ord :: Ordering) where
  demoteOrdering :: Proxy ord -> Ordering

instance DemoteOrdering 'LT where
  demoteOrdering _ = LT

instance DemoteOrdering 'EQ where
  demoteOrdering _ = EQ

instance DemoteOrdering 'GT where
  demoteOrdering _ = GT

getFirst :: L.HGetFirst x xs => HSortedList xs -> x
getFirst (HSortedList l) = L.hgetFirst l

class CGetAll x xs where
  getAll :: HSortedList xs -> [x]

instance CGetAll x '[] where
  getAll _ = []

instance (CGetAll x xs) => CGetAll x (x ': xs) where
  getAll (HSortedList (L.HCons x xs)) = x : getAll @x (HSortedList xs)

instance {-# OVERLAPS #-} (CGetAll x xs) => CGetAll x (y ': xs) where
  getAll (HSortedList (L.HCons _ tail')) = getAll @x (HSortedList tail')

type family Remove (x :: Type) (xs :: [Type]) :: [Type] where
  Remove x (y ': xs) = Remove' (x == y) x (y ': xs)

type family Remove' (eq :: Bool) (x :: Type) (xs :: [Type]) :: [Type] where
  Remove' 'True x (y ': xs) = xs
  Remove' 'False x (y ': xs) = y ': Remove x xs

class CRemove (x :: Type) (xs :: [Type]) (out :: [Type]) | x xs -> out where
  remove :: Proxy x -> HSortedList xs -> HSortedList out

instance (CRemove' e x (y ': xs) out, (x == y) ~ e) => CRemove x (y ': xs) out where
  remove = remove' (Proxy :: Proxy e)

class CRemove' (eq :: Bool) (x :: Type) (xs :: [Type]) (out :: [Type]) | eq x xs -> out where
  remove' :: Proxy eq -> Proxy x -> HSortedList xs -> HSortedList out

instance CRemove' 'True x (y ': xs) xs where
  remove' _ _ (HSortedList (L.HCons _ tail')) = HSortedList tail'

instance (CRemove x xs out) => CRemove' 'False x (y ': xs) (y ': out) where
  remove' _ p (HSortedList (L.HCons y tail')) =
    HSortedList $
      L.HCons y $
        unwrap
          (remove p (HSortedList tail'))

type MergeAll :: forall k. [[k]] -> [k]
type family MergeAll xs where
  MergeAll (x ': '[]) = x
  MergeAll (x ': y ': xs) = MergeAll (Merge x y ': xs)

type family Merge (xs :: [k]) (ys :: [k]) :: [k] where
  Merge '[] ys = ys
  Merge (x ': xs) '[] = x ': xs
  Merge (x ': xs) (y ': ys) = Merge' (CmpToSymbol x y) (x ': xs) (y ': ys)

type Merge' :: forall k. Ordering -> [k] -> [k] -> [k]
type family Merge' ord xs ys where
  Merge' 'LT (x ': xs) (y ': ys) = x ': Merge xs (y ': ys)
  Merge' 'EQ (x ': xs) (y ': ys) = x ': Merge xs (y ': ys)
  Merge' 'GT (x ': xs) (y ': ys) = y ': Merge (x ': xs) ys

class CMerge xs ys out | xs ys -> out where
  merge :: HSortedList xs -> HSortedList ys -> HSortedList out

instance CMerge '[] ys ys where
  merge _ ys = ys

instance CMerge (x ': xs) '[] (x ': xs) where
  merge xs _ = xs

instance (CMerge' (CmpToSymbol x y) (x ': xs) (y ': ys) out) => CMerge (x ': xs) (y ': ys) out where
  merge = merge' (Proxy :: (Proxy (CmpToSymbol x y)))

class CMerge' ord xs ys out | ord xs ys -> out where
  merge' :: Proxy ord -> HSortedList xs -> HSortedList ys -> HSortedList out

instance (CMerge xs (y ': ys) out) => CMerge' 'LT (x ': xs) (y ': ys) (x ': out) where
  merge' _ (HSortedList (L.HCons x xs)) ys =
    HSortedList $ L.HCons x (unwrap (merge (HSortedList xs) ys))

instance (CMerge xs (y ': ys) out) => CMerge' 'EQ (x ': xs) (y ': ys) (x ': out) where
  merge' _ (HSortedList (L.HCons x xs)) ys =
    HSortedList $ L.HCons x (unwrap (merge (HSortedList xs) ys))

instance (CMerge (x ': xs) ys out) => CMerge' 'GT (x ': xs) (y ': ys) (y ': out) where
  merge' _ xs (HSortedList (L.HCons y ys)) =
    HSortedList $ L.HCons y (unwrap (merge xs (HSortedList ys)))

class CInsert (ord :: Ordering) x xs out | ord x xs -> out where
  insert' :: Proxy ord -> x -> HSortedList xs -> HSortedList out

instance CInsert ord x '[] '[x] where
  insert' _ x' _ = HSortedList . L.hcons x' $ L.hnil

instance (CInsert (CmpToSymbol x z) x (z ': xs) res) => CInsert 'GT x (y ': z ': xs) (y ': res) where
  insert' _ x' (HSortedList (L.HCons y xs)) =
    HSortedList $
      L.hcons
        y
        (insert' (Proxy :: Proxy (CmpToSymbol x z)) x' (HSortedList xs) & unwrap)

instance CInsert 'GT x (y ': '[]) (y : x ': '[]) where
  insert' _ x' (HSortedList (L.HCons y' _)) =
    HSortedList $ L.hcons y' (L.hcons x' L.hnil)

instance CInsert 'LT x (y ': t) (x : y ': t) where
  insert' _ x' (HSortedList tail') = HSortedList $ L.hcons x' tail'

instance CInsert 'EQ x (y ': t) (x : y ': t) where
  insert' _ x' (HSortedList tail') = HSortedList $ L.hcons x' tail'

type family GetInitialOrd x xs :: Ordering where
  GetInitialOrd x (y ': xs) = CmpToSymbol x y
  GetInitialOrd _ _ = 'LT

insert ::
  forall x xs out o.
  (GetInitialOrd x xs ~ o, CInsert o x xs out) =>
  x ->
  HSortedList xs ->
  HSortedList out
insert = insert' (Proxy :: Proxy o)
