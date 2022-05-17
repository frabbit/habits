{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Veins.Test.HSpec where

passTrue :: forall x. (x ~ 'True) => IO ()
passTrue = pure ()