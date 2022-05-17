module Veins.Test.MockSpec where

import Control.Lens (makeLenses, Lens', lens)
import qualified Control.Lens as L
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import Data.Function ((&))
import GHC.Conc (atomically, newTVar, newTVarIO)
import Test.Hspec (shouldBe)
import qualified Test.Hspec as HS
import UnliftIO (modifyTVar)
import Utils (shouldBeIO)
import qualified Veins.Data.HList as HL
import qualified Veins.Data.Type.Function as F
import Veins.Test.Mock (MkSpy (mkSpy), MockifyArb (mockifyArb), getSpyArgs, getSpyArgsIO, getSpyCalls, getSpyCallsIO, mapCaptureForSpy, mkSpyIO, mockReturn, mockify, withSpy)
import Haskus.Utils.Variant.Excepts (Excepts, runE, pattern VRight, pattern VLeft, failureE)
import Haskus.Utils.Variant.VEither.Orphans ()
import Haskus.Utils.Variant (toVariant)

data Simple = MkSimple
  { _exec :: Int -> String
  }

makeLenses ''Simple

data Monadic m = MkMonadic
  { _execM :: Int -> m String
  }

makeLenses ''Monadic

data WithError = MkExcepts
  {_execErr :: Int -> Excepts '[()] IO String }


makeLenses ''WithError

spec :: HS.Spec
spec = do
  HS.describe "mockify" $ do
    HS.it "should work together with simple functions" $ do
      let mock = mockify MkSimple & L.set exec (const "hi")
      (mock & L.view exec) 1 `shouldBe` "hi"
      (mock & L.view exec) 2 `shouldBe` "hi"
    HS.it "should work with excepts based functions" $ do
      let mock = mockify MkExcepts & L.set execErr (const $ pure "hi")
      r <- runE $ (mock & L.view execErr) 1
      r `shouldBe` VRight "hi"
  HS.describe "mockifyArb" $ do
    HS.it "should work with monadic functions" $ do
      (spy, mock) <- mockifyArb MkMonadic & withSpy execM
      mock & L.view execM $ 1
      (fmap length . getSpyCallsIO $ spy) `shouldBeIO` 1
    HS.it "should work with excepts based functions" $ do
      (spy, mock) <- mockifyArb MkExcepts & withSpy execErr
      runE $ mock & L.view execErr $ 1
      (fmap length . getSpyCallsIO $ spy) `shouldBeIO` 1
  HS.describe "withSpy" $ do
    HS.it "should work with monadic functions" $ do
      (spy, mock) <- mockify (MkMonadic @IO) & L.set execM (\_ -> pure "hi") & withSpy execM
      res <- (mock & L.view execM) 1
      res `shouldBe` "hi"
      getSpyCallsIO spy `shouldBeIO` [(HL.HCons 1 HL.HNil, "hi" :: String)]
    HS.it "should work with excepts based functions" $ do
      (spy, mock) <- mockify MkExcepts & L.set execErr (const $ pure "hi") & withSpy execErr
      runE $ (mock & L.view execErr) 1
      getSpyCallsIO spy `shouldBeIO` [(HL.HCons 1 HL.HNil, "hi" :: String)]
    HS.it "should work with excepts based functions that produce an error" $ do
      (spy, mock) <- mockify MkExcepts & L.set execErr (const $ failureE ()) & withSpy execErr
      r <- runE $ (mock & L.view execErr) 1
      r `shouldBe` (VLeft . toVariant $ ())
      getSpyArgsIO spy `shouldBeIO` [HL.HCons 1 HL.HNil]
      getSpyCallsIO spy `shouldBeIO` []
  HS.describe "Veins.Test.MockReturn" $ do
    HS.it "should work with simple functions" $ do
      let mock = mockify MkSimple & L.over exec (mockReturn "hi")
      (mock & L.view exec) 1 `shouldBe` "hi"
      (mock & L.view exec) 2 `shouldBe` "hi"
    HS.it "should work with monadic functions" $ do
      let mock = mockify MkMonadic & L.over execM (mockReturn $ pure "hi")
      (mock & L.view execM) 1 `shouldBe` Just "hi"
      (mock & L.view execM) 2 `shouldBe` Just "hi"
  HS.describe "mkSpyIO" $ do
    let f1 :: String -> IO Int
        f1 s = pure (length s)
    let f2 :: String -> Int -> IO Int
        f2 s i = pure $ length s + i
    HS.it "should capture the call arguments in case of an error" $ do
      let f :: String -> ExceptT () IO Int
          f _ = throwE ()
      (v, spy) <- mkSpyIO f
      _ <- runExceptT $ spy "foo"
      getSpyArgsIO v `shouldBeIO` [HL.HCons "foo" HL.hnil]
      getSpyCallsIO v `shouldBeIO` []
    HS.it "should work with all monads having MonadIO instances" $ do
      let f :: String -> ExceptT () IO Int
          f s = pure (length s)
      (v, spy) <- mkSpyIO f
      l <- runExceptT $ spy "foo"
      getSpyCallsIO v `shouldBeIO` [(HL.HCons "foo" HL.hnil, 3)]
      l `shouldBe` Right 3
    HS.it "should be return the original result" $ do
      (_, spy) <- mkSpyIO f1
      l <- spy "foo"
      l `shouldBe` 3
    HS.it "should be compatible with mapCaptureForSpy" $ do
      (v, captured) <- mkSpyIO f1
      let spy = mkSpy f1 (mapCaptureForSpy captured)
      l <- spy "foo"
      getSpyCallsIO v `shouldBeIO` [(HL.HCons "foo" HL.hnil, 3)]
      l `shouldBe` 3
    HS.it "should track all calls" $ do
      (v, spy) <- mkSpyIO f1
      _ <- spy "hello"
      _ <- spy "foo"
      getSpyCallsIO v `shouldBeIO` [(HL.HCons "hello" HL.hnil, 5), (HL.HCons "foo" HL.hnil, 3)]
    HS.it "should work with one argument functions" $ do
      (v, spy) <- mkSpyIO f1
      _ <- spy "hello"
      getSpyCallsIO v `shouldBeIO` [(HL.HCons "hello" HL.hnil, 5)]
    HS.it "should work with multiple argument functions" $ do
      (v, spy) <- liftIO $ mkSpyIO f2
      _ <- spy "hello" 5
      getSpyCallsIO v `shouldBeIO` [("hello" `HL.HCons` (5 `HL.HCons` HL.hnil), 10)]
