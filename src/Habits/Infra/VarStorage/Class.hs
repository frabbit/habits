{-# LANGUAGE AllowAmbiguousTypes #-}
module Habits.Infra.VarStorage.Class where

import Habits.Prelude

import Veins.Data.Has (Has)
import Habits.Utils (applyFirst0M)
import Habits.Infra.VarStorage (GetVar, VarStorage)
import qualified Habits.Infra.VarStorage as VarStorage

class VarStorageM t m where
  getVar :: GetVar t m
  readVar :: m t

instance (MonadReader env m, Has (VarStorage t m) env) => VarStorageM t m where
  getVar = applyFirst0M VarStorage.getVar
  readVar = applyFirst0M VarStorage.readVar

