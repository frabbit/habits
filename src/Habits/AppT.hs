module Habits.AppT where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class
  ( MonadReader,
    ask,
    local,
  )
import Haskus.Utils.Variant.Excepts (Excepts, evalE)

newtype AppT env m a = AppT {unAppT :: ReaderT env m a} deriving (Functor, Applicative, Monad, MonadIO)

instance (Monad m) => MonadReader env (AppT env m) where
  ask = AppT ask
  local f ma = AppT $ local f (unAppT ma)

runAppT :: forall a m env. (Monad m) => env -> Excepts '[] (AppT env m) a -> m a
runAppT env = runReaderWithEnv . unwrap . evalE
  where
    unwrap = unAppT
    runReaderWithEnv r = runReaderT r env

runAppT' :: forall a m env. env -> AppT env m a -> m a
runAppT' env = runReaderWithEnv . unwrap
  where
    unwrap = unAppT
    runReaderWithEnv r = runReaderT r env