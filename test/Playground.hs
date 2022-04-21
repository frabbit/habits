{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}



module Playground where


import           Control.Monad.Error.Class      ( MonadError )
import           Control.Monad.Exception        ( CallTrace
                                                , EM
                                                , EMT(EMT)
                                                , Exception
                                                , Throws
                                                , runEMT
                                                , throw
                                                , unEMT
                                                )
import           Control.Monad.Exception.Catch  ( catch )
import           Control.Monad.Exception.Throws ( CheckedException )
import           Control.Monad.RWS              ( MonadIO(liftIO)
                                                , asks
                                                )
import           Control.Monad.Reader           ( Reader
                                                , ReaderT
                                                )
import           Control.Monad.Reader.Class     ( MonadReader
                                                , ask
                                                , local
                                                )
import           Data.Has                       ( Has
                                                , getter
                                                , modifier
                                                )
import           Data.Typeable                  ( Typeable )
import           Habits.Domain.AccountId        ( AccountId(AccountId) )
import           Habits.Domain.AccountNew       ( AccountNew )
import qualified Habits.Domain.AccountRepo     as AR
                                                ( AccountRepo(..)
                                                , Add
                                                , add
                                                )
import           Habits.Domain.AccountRepo      ( AddError )
import qualified Habits.Domain.AccountRepo.Class
                                               as ARC
                                                ( AccountRepo
                                                , add
                                                )
import           Habits.Domain.AccountRepo.InMemory
                                               as AccountRepoInMemory
import           Habits.UseCases.Register.Class ( Register
                                                , execute
                                                )
import           Habits.UseCases.Register.Live as RegisterLive
import           Test.Hspec

data MyError1 = MyError1
  deriving (Show, Typeable)

data MyError2 = MyError2
  deriving (Show, Typeable)

instance Exception MyError1
instance Exception MyError2


throwsNothing :: IO Int
throwsNothing = pure 1

test1Catched :: IO Int
test1Catched = runEMT $ test1 `catch` (\(e :: MyError1) -> pure 1)




test1 :: (Throws MyError1 e) => EMT e IO Int
test1 = throw MyError1

test2 :: (Throws MyError2 e) => EMT e IO Int
test2 = throw MyError2


test3 :: (Throws MyError1 e, Throws MyError2 e) => EMT e IO Int
test3 = do
  liftIO throwsNothing
  test1
  test2



spec :: Spec
spec = describe "RegisterSpec" $ do
  describe "read" $ do
    it "can parse integers" $ do
      read "10" `shouldBe` (10 :: Int)
