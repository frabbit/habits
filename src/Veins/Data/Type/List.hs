{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Veins.Data.Type.List
  ( Remove,
    ContainsNone,
    RemoveAll,
    Reverse,
    Concat,
    Elem,
  )
where

import Data.Kind (Type)
import Data.Type.Bool (Not)
import Veins.Data.Type.Bool (And)

type family Contains (y :: a) (x :: [a]) :: z where
  Contains a '[] = 'False
  Contains a (a ': tail) = 'True
  Contains a (b ': tail) = Contains a tail

type family Remove (y :: a) (x :: [a]) :: z where
  Remove a '[] = '[]
  Remove a (a ': tail) = Remove a tail
  Remove a (b ': tail) = (b ': Remove a tail)

type family Concat (y :: [Type]) (x :: [Type]) :: z where
  Concat a '[] = a
  Concat '[] b = b
  Concat (a ': xs) ys = a ': Concat xs ys

type ContainsNot x y = Not (Contains x y)

type family ContainsAll (x :: [k]) (ys :: [k]) :: Bool where
  ContainsAll '[] _ = 'True
  ContainsAll _ '[] = 'True
  ContainsAll (a ': xs) ys = And (Contains a ys) (ContainsAll xs ys)

type family ContainsNone (x :: [k]) (ys :: [k]) :: Bool where
  ContainsNone '[] _ = 'True
  ContainsNone _ '[] = 'True
  ContainsNone (a ': xs) ys = And (ContainsNot a ys) (ContainsNone xs ys)

type family RemoveAll (x :: [k]) (ys :: [k]) :: [k] where
  RemoveAll '[] b = b
  RemoveAll _ '[] = '[]
  RemoveAll (a ': xs) ys = RemoveAll xs (Remove a ys)

type family Elem (t :: k) (ts :: [k]) :: Bool where
  Elem a '[] = 'False
  Elem a (a ': as) = 'True
  Elem a (b ': bs) = Elem a bs

type family Reverse (ts :: [k]) :: [k] where
  Reverse l = ReverseLoop l '[]

type family ReverseLoop (ts :: [k]) (acc :: [k]) :: [k] where
  ReverseLoop (a ': b) acc = ReverseLoop b (a ': acc)
  ReverseLoop '[] acc = acc
