{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.Infra.Memory.EmailServiceMemorySpec (spec) where

import Habits.Test.Prelude

import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import UnliftIO (newTVarIO, readTVarIO)
import Veins.Test.QuickCheck (sampleIO)
import Habits.Domain.EmailService (EmailService)
import Habits.Infra.Memory.EmailServiceMemory (mkEmailServiceMemoryUsingVar)
import Habits.Domain.EmailService.Class (sendMessage)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Utils (shouldBeIO)


type Env m = CE.MkSorted '[EmailService m]

envLayer :: forall n m . (MonadIO n, MonadIO m, _) => _ -> CE.ReaderCE '[] n (Env m)
envLayer = mkEmailServiceMemoryUsingVar

AppTH.mkBoilerplate "runApp" ''Env

run :: _ => _
run var x = do
  env <- runReaderT (envLayer var) CE.empty
  runApp env x

withVar :: _ => _
withVar app = do
  var <- liftIO $ newTVarIO []
  run var (app var)

spec :: Spec
spec = fdescribe "EmailServiceMemory" $ do
  describe "sendMessage should" $ do
    it "store should be empty in the beginning" $ withVar $ \var -> evalE $ S.do
      S.coerce $ readTVarIO var `shouldBeIO` []
    it "store message in variable" $ withVar $ \var -> evalE $ S.do
      msg <- S.liftIO sampleIO
      sendMessage msg
      S.coerce $ readTVarIO var `shouldBeIO` [msg]
    it "store all messages in variable" $ withVar $ \var -> evalE $ S.do
      (msg1, msg2) <- S.liftIO sampleIO
      sendMessage msg1
      sendMessage msg2
      S.coerce $ readTVarIO var `shouldBeIO` [msg1, msg2]
