{-# LANGUAGE AllowAmbiguousTypes #-}
module Veins.RecordDot.Utils where

import GHC.Records.Compat (setField, HasField)
import GHC.TypeLits (Symbol)

set :: forall (x::Symbol) r a. HasField x r a =>  a -> r -> r
set a r = setField @x r a
