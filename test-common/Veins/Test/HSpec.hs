{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Veins.Test.HSpec where

import Prelude

passTrue :: forall x. (x ~ 'True) => IO ()
passTrue = pure ()