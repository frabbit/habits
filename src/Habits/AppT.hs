module Habits.AppT where

import           Control.Monad.Reader           ( ReaderT (runReaderT) )
import           Control.Monad.Reader.Class     ( MonadReader
                                                , ask
                                                , local
                                                )
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Variant (Variant, preposterous)
import Data.Void (absurd)
import           Control.Monad.IO.Class         ( MonadIO )

newtype AppT env m a = AppT { unAppT :: ReaderT env m a } deriving (Functor, Applicative, Monad, MonadIO)

instance (Monad m) => MonadReader env (AppT env m) where
  ask = AppT ask
  local f ma = AppT $ local f (unAppT ma)

eliminate :: (Monad m) => ExceptT (Variant '[]) m a -> m a
eliminate = fmap convert . runExceptT
 where
  convert (Right r) = r
  convert (Left  l) = (absurd . preposterous) l

runAppT :: forall a m env . (Monad m) => env -> ExceptT (Variant '[]) (AppT env m) a -> m a
runAppT env = runReaderWithEnv . unwrap . eliminate
 where
  unwrap = unAppT
  runReaderWithEnv r = runReaderT r env

runAppT' :: forall a m env . env -> AppT env m a -> m a
runAppT' env = runReaderWithEnv . unwrap
 where
  unwrap = unAppT
  runReaderWithEnv r = runReaderT r env