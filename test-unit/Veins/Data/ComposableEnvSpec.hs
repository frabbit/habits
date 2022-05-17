{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use let" #-}
{-# HLINT ignore "Redundant CE.return" #-}

module Veins.Data.ComposableEnvSpec where

import Control.Monad.Reader
  ( ReaderT (ReaderT, runReaderT),
    asks,
  )
import Data.Function ((&))
import Test.Hspec
  ( Spec,
    describe,
    focus,
    it,
    shouldBe,
  )
import Veins.Data.ComposableEnv
  ( ComposableEnv,
    addLayer,
    chainFromEnv,
    empty,
    expandEnv,
    expandEnvBy,
    expandLayer,
    get,
    insert,
    provideAll',
    provideAndChainLayer,
    provideLayer,
    provideLayer',
    remove,
    union,
  )
import qualified Veins.Data.ComposableEnv as CE
import Veins.Data.ToSymbol (type ToSymbol)

data A = A deriving (Show, Eq)

data B = B deriving (Show, Eq)

data C = C deriving (Show, Eq)

data D = D deriving (Show, Eq)

data E = E deriving (Show, Eq)

type instance ToSymbol A = "A"

type instance ToSymbol B = "B"

type instance ToSymbol C = "C"

type instance ToSymbol D = "D"

type instance ToSymbol E = "E"

data K = K Int deriving (Show, Eq)

data L = L Int deriving (Show, Eq)

data M = M Int deriving (Show, Eq)

type instance ToSymbol K = "K"

type instance ToSymbol L = "L"

type instance ToSymbol M = "M"

spec :: Spec
spec = describe "ComposableEnv" $ do
  describe "insert should" $ do
    it "insert a single new value into env" $ do
      let foo = empty & insert A
      show foo `shouldBe` "A #: []"
    it "insert multiple new values into env" $ do
      let foo = empty & insert A & insert B
      show foo `shouldBe` "A #: B #: []"
  describe "union should" $ do
    it "merge empty envs" $ do
      empty `union` empty `shouldBe` empty
    it "merge empty with non empty envs" $ do
      insert A empty `union` empty `shouldBe` insert A empty
      empty `union` insert A empty `shouldBe` insert A empty
    it "merge non empty envs" $ do
      insert A empty `union` empty `shouldBe` insert A empty
      empty `union` insert A empty `shouldBe` insert A empty
  describe "get" $ do
    it
      "should return the correct value regardless of how many elements the env contains"
      $ do
        get @A (insert A empty) `shouldBe` A
        get @A (insert A . insert B $ empty) `shouldBe` A
        get @A (insert B . insert A $ empty) `shouldBe` A

  describe "remove" $ do
    it "should remove value from env " $ do
      remove @A (insert A empty) `shouldBe` empty
      remove @A (insert A . insert B $ empty) `shouldBe` insert B empty
      remove @A (insert B . insert A $ empty) `shouldBe` insert B empty
  describe "addLayer should" $ do
    it "combine layers from two computations" $ do
      let layerA :: ReaderT (ComposableEnv '[]) IO (ComposableEnv '[A])
          layerA = pure $ empty & insert A
          layerB :: ReaderT (ComposableEnv '[]) IO (ComposableEnv '[B])
          layerB = pure $ empty & insert B
          app = addLayer layerA layerB
      r <- runReaderT app empty
      r `shouldBe` (empty & insert A & insert B)
    it "create a computation that requires the environment from both computations" $ do
      let layerA :: ReaderT (ComposableEnv '[A]) IO (ComposableEnv '[])
          layerA = pure empty
          layerB :: ReaderT (ComposableEnv '[B]) IO (ComposableEnv '[])
          layerB = pure empty
          app = addLayer layerA layerB
      r <- runReaderT app $ empty & insert A & insert B
      r `shouldBe` empty
    it "create a computation that requires the environment from both computations" $ do
      let layerA :: ReaderT (ComposableEnv '[A]) IO (ComposableEnv '[C])
          layerA = pure $ empty & insert C
          layerB :: ReaderT (ComposableEnv '[B]) IO (ComposableEnv '[D])
          layerB = pure $ empty & insert D
          app = addLayer layerA layerB
      r <- runReaderT app $ empty & insert A & insert B
      r `shouldBe` (empty & insert C & insert D)
  describe "expandLayer" $ do
    it "should expand consumed and returned environment with no dependency when set is empty" $ do
      let layer :: ReaderT (ComposableEnv '[]) IO (ComposableEnv '[B])
          layer = pure $ empty & insert B
          app :: ReaderT (ComposableEnv '[]) IO (ComposableEnv '[B])
          app = expandLayer @'[] layer
      r <- runReaderT app empty
      r `shouldBe` (empty & insert B)
    it "should expand consumend and returned environment with one dependency when set contains one type" $ do
      let layer :: ReaderT (ComposableEnv '[]) IO (ComposableEnv '[B])
          layer = pure $ empty & insert B
          app :: ReaderT (ComposableEnv '[A]) IO (ComposableEnv '[A, B])
          app = expandLayer @'[A] layer
      r <- runReaderT app $ empty & insert A
      r `shouldBe` (empty & insert A & insert B)
    it "should expand consumed environment and returned environment with multiple dependencies when set contains multiple types" $ do
      let layer :: ReaderT (ComposableEnv '[A]) IO (ComposableEnv '[B])
          layer = pure $ empty & insert B
          app :: ReaderT (ComposableEnv '[A, C, D]) IO (ComposableEnv '[B, C, D])
          app = expandLayer @'[C, D] layer
      r <- runReaderT app $ empty & insert A & insert C & insert D
      r `shouldBe` (empty & insert B & insert C & insert D)
  describe "chainFromEnv" $ do
    it "should pass one dependency from the environment to result" $ do
      let layer :: ReaderT (ComposableEnv '[A]) IO (ComposableEnv '[])
          layer = pure empty
          app = chainFromEnv @'[A] layer
      r <- runReaderT app $ empty & insert A
      r `shouldBe` (empty & insert A)
    it "should pass mulitple dependencies of the environment to the result" $ do
      let layer :: ReaderT (ComposableEnv '[A, B]) IO (ComposableEnv '[])
          layer = pure empty
          app = chainFromEnv @'[A, B] layer
      r <- runReaderT app $ empty & insert A & insert B
      r `shouldBe` (empty & insert A & insert B)
    it "should retain already existing dependencies in the result" $ do
      let layer :: ReaderT (ComposableEnv '[B]) IO (ComposableEnv '[A])
          layer = pure $ empty & insert A
          app = chainFromEnv @'[B] layer
      r <- runReaderT app $ empty & insert B
      r `shouldBe` (empty & insert A & insert B)
  describe "expandEnvBy" $ do
    it "should expand the env of an empty computation" $ do
      let layer :: ReaderT (ComposableEnv '[]) IO ()
          layer = pure ()
          app = expandEnvBy @'[A] layer
      r <- runReaderT app $ empty & insert A
      r `shouldBe` ()
    it "should expand the env of an existing computation" $ do
      let layer :: ReaderT (ComposableEnv '[A]) IO ()
          layer = pure ()
          app = expandEnvBy @'[B] layer
      r <- runReaderT app $ empty & insert A & insert B
      r `shouldBe` ()
  describe "expandEnv" $ do
    it "should expand the env of an empty computation" $ do
      let layer :: ReaderT (ComposableEnv '[]) IO ()
          layer = pure ()
          app = expandEnv @'[A] layer
      r <- runReaderT app $ empty & insert A
      r `shouldBe` ()
    it "should expand the env of an existing computation" $ do
      let layer :: ReaderT (ComposableEnv '[A]) IO ()
          layer = pure ()
          app = expandEnv @'[A, B] layer
      r <- runReaderT app $ empty & insert A & insert B
      r `shouldBe` ()
  describe "provideAndChainLayer should" $ do
    it "use dependency from layer to shrink input env and grow layer output" $ do
      let layer0 :: ReaderT (ComposableEnv '[A]) IO (ComposableEnv '[])
          layer0 = pure empty
          layer1 :: ReaderT (ComposableEnv '[B]) IO (ComposableEnv '[A])
          layer1 = pure $ empty & insert A

          out :: ReaderT (ComposableEnv '[B]) IO (ComposableEnv '[A])
          out = provideAndChainLayer layer1 layer0
      r <- runReaderT out (empty & insert B)
      r `shouldBe` (empty & insert A)
  describe "provideLayer should" $ do
    it "remove one dependency from a computation that requires exactly that" $ do
      let app :: ReaderT (ComposableEnv '[A]) IO ()
          app = pure ()
          layer :: ReaderT (ComposableEnv '[]) IO (ComposableEnv '[A])
          layer = pure $ empty & insert A
          app' :: ReaderT (ComposableEnv '[]) IO ()
          app' = provideLayer layer app

      r <- runReaderT app' empty
      r `shouldBe` ()
    it "remove multiple dependencies from a computation that requires exactly those" $ do
      let app :: ReaderT (ComposableEnv '[A, B]) IO ()
          app = pure ()
          layer :: ReaderT (ComposableEnv '[]) IO (ComposableEnv '[A, B])
          layer = pure $ empty & insert A & insert B
          app' :: ReaderT (ComposableEnv '[]) IO ()
          app' = provideLayer layer app

      r <- runReaderT app' empty
      r `shouldBe` ()
    it "remove a dependency from a computation that requires others besides that" $ do
      let app :: ReaderT (ComposableEnv '[A, B]) IO ()
          app = pure ()
          layer :: ReaderT (ComposableEnv '[]) IO (ComposableEnv '[A])
          layer = pure $ empty & insert A
          app' :: ReaderT (ComposableEnv '[B]) IO ()
          app' = provideLayer layer app

      r <- runReaderT app' (empty & insert B)
      r `shouldBe` ()
    it "remove multiple dependencies from a computation that requires others besides them" $ do
      let app :: ReaderT (ComposableEnv '[A, B, C]) IO ()
          app = pure ()
          layer :: ReaderT (ComposableEnv '[]) IO (ComposableEnv '[A, B])
          layer = pure $ empty & insert A & insert B
          app' :: ReaderT (ComposableEnv '[C]) IO ()
          app' = provideLayer layer app

      r <- runReaderT app' (empty & insert C)
      r `shouldBe` ()
    it "remove no dependencies from the computation when the layer provides no dependencies at all" $ do
      let app :: ReaderT (ComposableEnv '[A]) IO ()
          app = pure ()
          layer :: ReaderT (ComposableEnv '[]) IO (ComposableEnv '[])
          layer = pure empty
          app' :: ReaderT (ComposableEnv '[A]) IO ()
          app' = provideLayer layer app

      r <- runReaderT app' (empty & insert A)
      r `shouldBe` ()
    it "ignores additional dependencies from layer when computation doesn't require them" $ do
      let app :: ReaderT (ComposableEnv '[A]) IO ()
          app = pure ()
          layer :: ReaderT (ComposableEnv '[]) IO (ComposableEnv '[B])
          layer = pure $ empty & insert B
          app' :: ReaderT (ComposableEnv '[A]) IO ()
          app' = provideLayer' layer app

      r <- runReaderT app' (empty & insert A)
      r `shouldBe` ()
  describe "do syntax" $ do
    let k :: ReaderT (ComposableEnv '[K]) IO K
        k = asks (get @K)
        l :: ReaderT (ComposableEnv '[L]) IO L
        l = asks (get @L)
        m :: ReaderT (ComposableEnv '[M]) IO M
        m = asks (get @M)
    it "should work in simple cases" $ do
      let app :: ReaderT (ComposableEnv '[]) IO Int
          app = CE.do
            CE.return 1
      val <- runReaderT app empty
      val `shouldBe` 1
    it "should work with simple bind" $ do
      let app :: ReaderT (ComposableEnv '[]) IO Int
          app = CE.do
            x <- CE.return 1
            CE.return x
      val <- runReaderT app empty
      val `shouldBe` 1
    it "should work a real env" $ do
      let app :: ReaderT (ComposableEnv '[K]) IO Int
          app = CE.do
            K a <- k
            CE.return a
      val <- runReaderT app (empty & insert (K 1))
      val `shouldBe` 1
    it "should work with two binds" $ do
      let app :: ReaderT (ComposableEnv '[K, L]) IO Int
          app = CE.do
            K a <- k
            L b <- l
            CE.return $ a + b
      val <- runReaderT app (empty & insert (K 1) & insert (L 2))
      val `shouldBe` 3
    it "should work with three binds" $ do
      let app :: ReaderT (ComposableEnv '[K, L, M]) IO Int
          app = CE.do
            K a <- k
            L b <- l
            M c <- m
            CE.return $ a + b + c
      val <- runReaderT app (empty & insert (K 1) & insert (L 2) & insert (M 4))
      val `shouldBe` 7

  describe "provideAll'" $ do
    it "should accept a subset of a given env" $ do
      let f :: ComposableEnv '[C, D] -> ComposableEnv '[A, B, C, D]
          f =
            provideAll'
              (\(env :: ComposableEnv '[A, B, C, D]) -> env)
              (insert A . insert B $ empty)
      f (insert C . insert D $ empty)
        `shouldBe` (insert A . insert B . insert C . insert D $ empty)
    it "should accept an empty env" $ do
      let f :: ComposableEnv '[A, B] -> ComposableEnv '[A, B]
          f = provideAll' (\(env :: ComposableEnv '[A, B]) -> env) empty
      f (insert A . insert B $ empty) `shouldBe` (insert A . insert B $ empty)
    it "should accept a superset as env" $ do
      let f :: ComposableEnv '[] -> ()
          f = provideAll' (\(_ :: ComposableEnv '[]) -> ()) (empty & insert A)
      f empty `shouldBe` ()
