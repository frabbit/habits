{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Habits.Tools.MonadThrow where
import           Control.Monad.Exception        ( EMT(EMT), Exception, Throws )

class MonadThrow t where
  throw :: (Throws c e, Exception c) => c -> t e a



