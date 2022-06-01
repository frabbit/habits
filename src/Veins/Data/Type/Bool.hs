{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Veins.Data.Type.Bool
  ( Not,
    And,
    Or,
    type (==),
  )
where

import Prelude

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

type family And (x :: Bool) (y :: Bool) :: Bool where
  And 'True 'True = 'True
  And _ _ = 'False

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True _ = 'True
  Or _ 'True = 'True
  Or _ _ = 'False

type (==) :: k -> k -> Bool
type family a == b where
  f a == g b = f == g `And` a == b
  a == a = 'True
  _ == _ = 'False
