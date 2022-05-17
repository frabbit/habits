{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Veins.Data.HSortedListSpec where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Type.Equality (type (==))
import Debug.Trace (traceIO)
import GHC.TypeLits (AppendSymbol)
import Test.Hspec
  ( Spec,
    context,
    describe,
    it,
    shouldBe,
  )
import Veins.Data.HSortedList
  ( CRemove (remove),
    Insert,
    empty,
    getAll,
    getFirst,
    insert,
    merge,
  )
import Veins.Data.ToSymbol (ToSymbol)
import Veins.Test.HSpec (passTrue)

type instance ToSymbol (Repo m) = AppendSymbol "Repo" (ToSymbol m)

data A = A
  deriving (Show, Eq)

data B = B
  deriving (Show, Eq)

data C = C
  deriving (Show, Eq)

data D = D
  deriving (Show, Eq)

data E = E
  deriving (Show, Eq)

type instance ToSymbol A = "A"

type instance ToSymbol B = "B"

type instance ToSymbol C = "C"

type instance ToSymbol D = "D"

type instance ToSymbol E = "E"

data Repo m = Repo
  { f :: m Int
  }

spec :: Spec
spec = describe "HSortedList" $ do
  context "Insert" $ do
    it "should contain element order" $ do
      passTrue @(Insert Int '[Text] == '[Int, Text])
      passTrue @(Insert Text '[Int] == '[Int, Text])
      passTrue @(Insert Text '[Repo Maybe] == '[Repo Maybe, Text])
      passTrue @(Insert Int '[Repo Maybe] == '[Int, Repo Maybe])
      passTrue @(Insert (Repo Maybe) '[Int] == '[Int, Repo Maybe])

  context "insert" $ do
    it "should insert values correctly" $ do
      liftIO $ traceIO "hello"
      show (insert A . insert B $ empty) `shouldBe` "A #: B #: []"
      show (insert B . insert A $ empty) `shouldBe` "A #: B #: []"
      show (insert C . insert B . insert A $ empty)
        `shouldBe` "A #: B #: C #: []"
  context "remove" $ do
    it "should remove the term level value for a given type" $ do
      remove (Proxy :: Proxy B) (insert A . insert C . insert B $ empty) `shouldBe` (insert A . insert C $ empty)
      remove (Proxy :: Proxy B) (insert B empty) `shouldBe` empty
  context "getFirst" $ do
    it "should return the term level value for a given type" $ do
      getFirst @B (insert A . insert C . insert B $ empty) `shouldBe` B
      getFirst @A (insert A . insert C . insert B $ empty) `shouldBe` A
      getFirst @C (insert A . insert C . insert B $ empty) `shouldBe` C

  context "getAll" $ do
    it "should return all term level values for a given type" $ do
      getAll @A (insert A . insert A . insert B $ empty) `shouldBe` [A, A]
      getAll @A empty `shouldBe` []
      getAll @A (insert A . insert C $ empty) `shouldBe` [A]

  context "merge" $ do
    it "should merge two empty lists" $ do
      show (empty `merge` empty) `shouldBe` "[]"
    it "should merge empty list on the left" $ do
      show (empty `merge` insert A empty) `shouldBe` "A #: []"
    it "should merge empty list on the right" $ do
      show (insert A empty `merge` empty) `shouldBe` "A #: []"
    it "should merge non empty lists" $ do
      show ((insert C . insert B $ empty) `merge` (insert A . insert D $ empty))
        `shouldBe` "A #: B #: C #: D #: []"
    it "should merge non empty lists with different sizes" $ do
      show
        ( (insert C . insert B $ empty)
            `merge` (insert E . insert A . insert D $ empty)
        )
        `shouldBe` "A #: B #: C #: D #: E #: []"
