{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Veins.Data.ToSymbol where

import Data.ByteString (ByteString)
import Data.Data (Proxy (..))
import Data.Text (Text)
import GHC.TypeLits
  ( AppendSymbol,
    CmpSymbol,
    KnownSymbol,
    Symbol,
    symbolVal,
  )

type family ToSymbol (x :: k) :: Symbol

type instance ToSymbol Int = "Int"

type instance ToSymbol ByteString = "ByteString"

type instance ToSymbol [x] = "[" `AppendSymbol` ToSymbol x `AppendSymbol` "]"

type instance ToSymbol Text = "Text"

type instance ToSymbol Maybe = "Maybe"

type instance
  ToSymbol (Maybe x) =
    "Maybe (" `AppendSymbol` ToSymbol x `AppendSymbol` ")"

class ToSymbol' x where
  toSymbol' :: Proxy x -> String

instance (ToSymbol x ~ x', KnownSymbol x') => ToSymbol' x where
  toSymbol' :: Proxy x -> String
  toSymbol' _ = symbolVal (Proxy :: Proxy x')

type CmpToSymbol a b = CmpSymbol (ToSymbol a) (ToSymbol b)
