module Veins.Data.Has where

class Has e container where
  get :: container -> e