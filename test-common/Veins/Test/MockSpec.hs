module Veins.Test.MockSpec where

import qualified Control.Lens as L
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (runExceptT, throwE, ExceptT)
import Data.Function ((&))
import Test.Hspec (shouldBe)
import qualified Test.Hspec as HS
import Utils (shouldBeIO)
import qualified Veins.Data.HList as HL
import Veins.Test.Mock (MkSpy (mkSpy), MockifyArb (mockifyArb), getSpyArgsIO, getSpyCallsIO, mapCaptureForSpy, mkSpyIO, mockReturn, mockify, withSpy)
import Haskus.Utils.Variant.Excepts (Excepts, runE, pattern VRight, pattern VLeft, failureE)
import Haskus.Utils.Variant.VEither.Orphans ()
import Haskus.Utils.Variant (toVariant)
import Veins.Control.Lens.Utils (makeLensesWithSuffixL)


data Simple = MkSimple
  { exec :: Int -> String
  }

makeLensesWithSuffixL ''Simple

data Monadic m = MkMonadic
  { execMon :: Int -> m String
  }

makeLensesWithSuffixL ''Monadic

data WithError = MkExcepts
  {execErr :: Int -> Excepts '[()] IO String }


makeLensesWithSuffixL ''WithError

spec :: HS.Spec
spec = do
  HS.describe "mockify" $ do
    HS.it "should work together with simple functions" $ do

      let mock = mockify MkSimple & \r -> r{ exec = const "hi" }
      mock.exec 1 `shouldBe` "hi"
      mock.exec 2 `shouldBe` "hi"
    HS.it "should work with excepts based functions" $ do
      let mock = mockify MkExcepts & L.set execErrL (const $ pure "hi")
      r <- runE $ mock.execErr 1
      r `shouldBe` VRight "hi"
  HS.describe "mockifyArb" $ do
    HS.it "should work with monadic functions" $ do
      (spy, mock) <- mockifyArb MkMonadic & withSpy execMonL
      mock & L.view execMonL $ 1
      (fmap length . getSpyCallsIO @IO $ spy) `shouldBeIO` 1
    HS.it "should work with excepts based functions" $ do
      (spy, mock) <- mockifyArb MkExcepts & withSpy execErrL
      runE $ mock & L.view execErrL $ 1
      (fmap length . getSpyCallsIO $ spy) `shouldBeIO` 1
  HS.describe "withSpy" $ do
    HS.it "should work with monadic functions" $ do
      (spy, mock) <- mockify (MkMonadic @IO) & L.set execMonL (\_ -> pure "hi") & withSpy execMonL
      res <- mock.execMon 1
      res `shouldBe` "hi"
      getSpyCallsIO spy `shouldBeIO` [(HL.HCons 1 HL.HNil, "hi" :: String)]
    HS.it "should work with excepts based functions" $ do
      (spy, mock) <- mockify MkExcepts & L.set execErrL (const $ pure "hi") & withSpy execErrL
      runE $ mock.execErr 1
      getSpyCallsIO spy `shouldBeIO` [(HL.HCons 1 HL.HNil, "hi" :: String)]
    HS.it "should work with excepts based functions that produce an error" $ do
      (spy, mock) <- mockify MkExcepts & L.set execErrL (const $ failureE ()) & withSpy execErrL
      r <- runE $ mock.execErr 1
      r `shouldBe` (VLeft . toVariant $ ())
      getSpyArgsIO spy `shouldBeIO` [HL.HCons 1 HL.HNil]
      getSpyCallsIO spy `shouldBeIO` []
  HS.describe "Veins.Test.MockReturn" $ do
    HS.it "should work with simple functions" $ do
      let mock = mockify MkSimple & L.over execL (mockReturn "hi")
      mock.exec 1 `shouldBe` "hi"
      mock.exec 2 `shouldBe` "hi"
    HS.it "should work with monadic functions" $ do
      let mock = mockify MkMonadic & L.over execMonL (mockReturn $ pure "hi")
      mock.execMon 1 `shouldBe` Just "hi"
      mock.execMon 2 `shouldBe` Just "hi"
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
