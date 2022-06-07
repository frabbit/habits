{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Habits.Infra.Memory.EmailServiceMemorySpec (spec) where

import Habits.Test.Prelude

import qualified Veins.Data.ComposableEnv as CE
import qualified Veins.Test.AppTH as AppTH
import Veins.Test.QuickCheck (sampleIO)
import Habits.Domain.EmailService (EmailService)
import Habits.Infra.Memory.EmailServiceMemory (mkEmailServiceMemory)
import Habits.Domain.EmailService.Class (sendMessage)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Habits.Infra.VarStorage.Class (VarStorageM(readVar))
import Habits.Domain.EmailMessage (EmailMessage)
import Habits.Infra.VarStorage (VarStorage)
import Habits.Infra.VarStorage.Live (mkVarStorageLive)
import Utils (shouldBeIO)


type Env m = CE.MkSorted '[EmailService m, VarStorage [EmailMessage] m]

envLayer :: forall n m . (MonadIO n, MonadIO m, _) => CE.ReaderCE '[] n (Env m)
envLayer = mkEmailServiceMemory <<-&& mkVarStorageLive @[EmailMessage] []

AppTH.mkBoilerplate "runApp" ''Env

run :: () => (App_runApp b -> IO b)
run app = do
  env <- runReaderT envLayer CE.empty
  runApp env app

spec :: Spec
spec = describe "EmailServiceMemory" $ do
  describe "sendMessage should" $ do
    it "store should be empty in the beginning" . run . evalE$ S.do
      S.lift $ readVar @[EmailMessage] `shouldBeIO` []
    it "store message in variable" . run . evalE $ S.do
      msg <- S.liftIO sampleIO
      sendMessage msg
      S.lift $ readVar @[EmailMessage] `shouldBeIO` [msg]
    it "store all messages in variable" $ run  . evalE $ S.do
      (msg1, msg2) <- S.liftIO sampleIO
      sendMessage msg1
      sendMessage msg2
      S.lift $ readVar @[EmailMessage] `shouldBeIO` [msg1, msg2]
