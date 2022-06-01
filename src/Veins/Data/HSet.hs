{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Veins.Data.HSet
  ( HSet,
    empty,
    toList,
    get,
    type Excluding,
    type Union,
    type Difference,
    type Intersection,
    type IsSubset,
    CInsert (..),
    CUnion (..),
    CRemove (..),
    CExcluding (..),
    CIntersection (..),
    excludingSet,
    UnionC,
    ExcludingC,
    IntersectionC,
  )
where

import Prelude

import Data.Kind
  ( Constraint,
    Type,
  )
import Data.Proxy (Proxy (Proxy))
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import qualified Veins.Data.HList as HL
import Veins.Data.ToSymbol (CmpToSymbol)

newtype HSet e = HSet (HL.HList e)

instance (Show (HL.HList x)) => Show (HSet x) where
  show = show . unwrap

instance (Eq (HL.HList x)) => Eq (HSet x) where
  a == b = unwrap a == unwrap b

toList :: HSet xs -> HL.HList xs
toList = unwrap

unwrap :: HSet e -> HL.HList e
unwrap (HSet e) = e

empty :: HSet '[]
empty = HSet HL.hnil

singleton :: x -> HSet '[x]
singleton x = HSet $ HL.hcons x HL.hnil

type Remove :: Type -> [Type] -> [Type]
type family Remove x xs where
  Remove x (y : xs) = If (x == y) xs (y : Remove x xs)

type CRemove :: Type -> [Type] -> [Type] -> Constraint
class CRemove x xs out | x xs -> out where
  remove :: HSet xs -> HSet out

instance (CRemove' e x (y : xs) out, (x == y) ~ e) => CRemove x (y ': xs) out where
  remove = remove' (Proxy :: Proxy e) (Proxy :: Proxy x)

type CRemove' :: Bool -> Type -> [Type] -> [Type] -> Constraint
class CRemove' eq x xs out | eq x xs -> out where
  remove' :: Proxy eq -> Proxy x -> HSet xs -> HSet out

instance CRemove' 'True x (y : xs) xs where
  remove' _ _ (HSet (HL.HCons _ tail')) = HSet tail'

instance (CRemove x xs out) => CRemove' 'False x (y ': xs) (y ': out) where
  remove' _ _ (HSet (HL.HCons y tail')) =
    HSet $ HL.HCons y $ unwrap (remove @x (HSet tail'))

class CInsert x xs out | x xs -> out where
  insert :: x -> HSet xs -> HSet out

instance CInsert x '[] '[x] where
  insert x' _ = singleton x'

instance (CInsert' (CmpToSymbol x y) x (y : ys) out) => CInsert x (y : ys) out where
  insert = insert' @(CmpToSymbol x y)

class CInsert' (ord :: Ordering) x xs out | ord x xs -> out where
  insert' :: x -> HSet xs -> HSet out

instance (CInsert x xs out) => CInsert' 'GT x (y : xs) (y : out) where
  insert' x' (HSet (HL.HCons y xs)) =
    HSet $ HL.hcons y (unwrap (insert x' (HSet xs)))

instance CInsert' 'EQ x (y : xs) (x : xs) where
  insert' x' (HSet (HL.HCons _ ys)) = HSet $ HL.hcons x' ys

instance CInsert' 'LT x (y : xs) (x : y : xs) where
  insert' x' ys = HSet $ HL.hcons x' (unwrap ys)

type family Union (xs :: [k]) (ys :: [k]) :: [k] where
  Union '[] ys = ys
  Union (x ': xs) '[] = x ': xs
  Union (x ': xs) (y ': ys) = Union' (CmpToSymbol x y) (x ': xs) (y ': ys)

type Union' :: forall k. Ordering -> [k] -> [k] -> [k]
type family Union' ord xs ys where
  Union' 'LT (x ': xs) (y ': ys) = x ': Union xs (y ': ys)
  Union' 'EQ (x ': xs) (_ ': ys) = x ': Union xs ys
  Union' 'GT (x ': xs) (y ': ys) = y ': Union (x ': xs) ys

type IsSubset :: forall k. [k] -> [k] -> Bool
type family IsSubset xs ys where
  IsSubset '[] _ = 'True
  IsSubset _ '[] = 'False
  IsSubset (x : xs) (y : ys) = IsSubset' (CmpToSymbol x y) x y xs ys

type IsSubset' :: forall k. Ordering -> k -> k -> [k] -> [k] -> Bool
type family IsSubset' o x y xs ys where
  IsSubset' 'LT _ _ _ _ = 'False
  IsSubset' 'EQ _ _ xs ys = IsSubset xs ys
  IsSubset' 'GT x _ xs ys = IsSubset (x : xs) ys

type Difference :: [k] -> [k] -> [k]
type family Difference xs ys where
  Difference xs '[] = xs
  Difference '[] (y : ys) = (y : ys)
  Difference (x : xs) (y : ys) = Difference' (CmpToSymbol x y) x y xs ys

type Difference' :: Ordering -> k -> k -> [k] -> [k] -> [k]
type family Difference' ord x y xs ys where
  Difference' 'EQ _ _ xs ys = Difference xs ys
  Difference' 'LT x y xs ys = (x : Difference xs (y : ys))
  Difference' 'GT x y xs ys = (y : Difference (x : xs) ys)

type Intersection :: [k] -> [k] -> [k]
type family Intersection xs ys where
  Intersection xs '[] = '[]
  Intersection '[] (y : ys) = '[]
  Intersection (x : xs) (y : ys) = Intersection' (CmpToSymbol x y) x y xs ys

type Intersection' :: Ordering -> k -> k -> [k] -> [k] -> [k]
type family Intersection' ord x y xs ys where
  Intersection' 'EQ x x xs ys = (x : Intersection xs ys)
  Intersection' 'LT x y xs ys = Intersection xs (y : ys)
  Intersection' 'GT x y xs ys = Intersection (x : xs) ys

type Excluding :: [k] -> [k] -> [k]
type family Excluding xs ys where
  Excluding xs '[] = xs
  Excluding '[] x = '[]
  Excluding (x : xs) (y : ys) = Excluding' (CmpToSymbol x y) x y xs ys

type Excluding' :: Ordering -> k -> k -> [k] -> [k] -> [k]
type family Excluding' ord x y xs ys where
  Excluding' 'LT x y xs ys = (x : Excluding xs (y : ys))
  Excluding' 'GT x y xs ys = Excluding (x : xs) ys
  Excluding' 'EQ x y xs ys = Excluding xs ys

type IntersectionC :: [Type] -> [Type] -> [Type] -> Constraint
type family IntersectionC xs ys out where
  IntersectionC xs ys out = (CIntersection ys xs out, Intersection xs ys ~ out, ())

type UnionC :: [Type] -> [Type] -> [Type] -> Constraint
type family UnionC xs ys out where
  UnionC xs ys out = (CUnion xs ys out, Union xs ys ~ out, ())

type ExcludingC :: [Type] -> [Type] -> [Type] -> Constraint
type family ExcludingC xs ys out where
  ExcludingC xs ys out = (CExcluding ys xs out, Excluding xs ys ~ out, ())

class CUnion xs ys out | xs ys -> out where
  union :: HSet xs -> HSet ys -> HSet out

instance CUnion '[] ys ys where
  union _ ys = ys

instance CUnion (x ': xs) '[] (x ': xs) where
  union xs _ = xs

instance (CUnion' (CmpToSymbol x y) (x ': xs) (y ': ys) out) => CUnion (x ': xs) (y ': ys) out where
  union = union' @(CmpToSymbol x y)

class CUnion' ord xs ys out | ord xs ys -> out where
  union' :: HSet xs -> HSet ys -> HSet out

instance (CUnion xs (y ': ys) out) => CUnion' 'LT (x ': xs) (y ': ys) (x ': out) where
  union' (HSet (HL.HCons x xs)) ys =
    HSet $ HL.HCons x (unwrap (HSet xs `union` ys))

instance (CUnion xs ys out) => CUnion' 'EQ (x ': xs) (y ': ys) (y ': out) where
  union' (HSet (HL.HCons _ xs)) (HSet (HL.HCons y ys)) =
    HSet $ HL.HCons y (unwrap (HSet xs `union` HSet ys))

instance (CUnion (x ': xs) ys out) => CUnion' 'GT (x ': xs) (y ': ys) (y ': out) where
  union' xs (HSet (HL.HCons y ys)) =
    HSet $ HL.HCons y (unwrap (xs `union` HSet ys))

get :: forall x xs. (HL.HGetFirst x xs) => HSet xs -> x
get = HL.hgetFirst @x . unwrap

excludingSet :: forall xs ys. (CExcluding ys xs (Excluding xs ys)) => HSet xs -> HSet ys -> HSet (Excluding xs ys)
excludingSet xs _ = excluding @ys xs

type CExcluding :: [Type] -> [Type] -> [Type] -> Constraint
class CExcluding rs xs out | rs xs -> out where
  excluding :: HSet xs -> HSet out

instance CExcluding '[] xs xs where
  excluding xs = xs

instance CExcluding (r : rs) '[] '[] where
  excluding xs = xs

instance (CExcluding' (CmpToSymbol r x) r x rs xs out) => CExcluding (r : rs) (x : xs) out where
  excluding = excluding' @(CmpToSymbol r x) @r @x @rs

type CExcluding' :: Ordering -> Type -> Type -> [Type] -> [Type] -> [Type] -> Constraint
class CExcluding' ord r x rs xs out | ord r x rs xs -> out where
  excluding' :: HSet (x : xs) -> HSet out

instance (CExcluding rs (x : xs) out) => CExcluding' 'LT r x rs xs out where
  excluding' = excluding @rs

instance (CExcluding (r : rs) xs out) => CExcluding' 'GT r x rs xs (x : out) where
  excluding' (HSet (HL.HCons x xs)) = HSet $ HL.HCons x (unwrap $ excluding @(r : rs) (HSet xs))

instance (CExcluding rs xs out) => CExcluding' 'EQ r x rs xs out where
  excluding' (HSet (HL.HCons _ xs)) = excluding @rs (HSet xs)

type CIntersection :: [Type] -> [Type] -> [Type] -> Constraint
class CIntersection rs xs out | rs xs -> out where
  intersection :: HSet xs -> HSet out

instance CIntersection '[] xs '[] where
  intersection _ = empty

instance CIntersection (r : rs) '[] '[] where
  intersection _ = empty

instance (IsSorted (r : rs), CIntersection' (CmpToSymbol r x) r x rs xs out) => CIntersection (r : rs) (x : xs) out where
  intersection = intersection' @(CmpToSymbol r x) @r @x @rs

type CIntersection' :: Ordering -> Type -> Type -> [Type] -> [Type] -> [Type] -> Constraint
class CIntersection' ord r x rs xs out | ord r x rs xs -> out where
  intersection' :: HSet (x : xs) -> HSet out

instance (CIntersection rs (x : xs) out) => CIntersection' 'LT r x rs xs out where
  intersection' = intersection @rs

instance (CIntersection (r : rs) xs out) => CIntersection' 'GT r x rs xs out where
  intersection' (HSet (HL.HCons _ xs)) = intersection @(r : rs) (HSet xs)

instance (CIntersection rs xs out) => CIntersection' 'EQ r x rs xs (x : out) where
  intersection' (HSet (HL.HCons x xs)) = HSet $ HL.hcons x (unwrap (intersection @rs (HSet xs)))

type IsSorted :: [k] -> Constraint
type family IsSorted xs where
  IsSorted (x : '[]) = ()
  IsSorted (x : y : xs) = (CmpToSymbol x y ~ 'LT, IsSorted (y : xs))
