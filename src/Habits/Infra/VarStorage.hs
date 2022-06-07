module Habits.Infra.VarStorage where

import Habits.Prelude
import qualified Veins.Data.Has as Has
import UnliftIO (TVar)
import Veins.Data.ToSymbol
import GHC.TypeLits (AppendSymbol)

type GetVar t m = m (TVar t)
type ReadVar t m = m t

data VarStorage t m = VarStorage {
  getVar :: GetVar t m,
  readVar :: ReadVar t m
}

getVarStorage :: forall t m n env. (Has.Has (VarStorage t m) env, MonadReader env n) => n (VarStorage t m)
getVarStorage = asks Has.get

type instance ToSymbol (VarStorage t m) = "VarStorage (" `AppendSymbol` ToSymbol t `AppendSymbol` ")"