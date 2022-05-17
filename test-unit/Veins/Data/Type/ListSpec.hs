{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}

module Veins.Data.Type.ListSpec where

import Veins.Test.HSpec (passTrue)
import Data.Proxy (Proxy (Proxy))
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Veins.Data.HList
  ( HList,
    (#:),
  )
import qualified Veins.Data.HList as HL
import Veins.Data.Type.Bool (type (==))
import qualified Veins.Data.Type.List as L

data A = A
  deriving (Show)

data B = B
  deriving (Show)

data C = C
  deriving (Show)

data D = D
  deriving (Show)

type ABCD = '[A, B, C, D]

type BD = '[B, D]

type AC = '[A, C]

abcd :: HList '[A, B, C, D]
abcd = A #: B #: C #: D #: HL.hnil

bd :: HList '[B, D]
bd = B #: D #: HL.hnil

ac :: HList '[A, C]
ac = A #: C #: HL.hnil

pass :: IO ()
pass = pure () :: IO ()

proxyTrue :: Proxy 'True
proxyTrue = Proxy

spec :: Spec
spec = describe "ListSpec" $ do
  describe "cons" $ do
    it "should append an element" $ do
      let l = A #: B #: HL.hnil

      show l `shouldBe` "A #: B #: []"

  describe "ContainsNone" $ do
    it
      "should return true if second list does not contain any element from first list"
      $ do
        passTrue @(L.ContainsNone '[] '[A, B] == 'True)
        passTrue @(L.ContainsNone '[] '[A, B] == 'True)
        passTrue @(L.ContainsNone '[A] '[A, B] == 'False)
        passTrue @(L.ContainsNone '[B] '[A, B] == 'False)
        passTrue @(L.ContainsNone '[C] '[A, B] == 'True)

  describe "RemoveAll" $ do
    it "should remove all elements of first list from second list" $ do
      passTrue @(L.RemoveAll '[] '[A, B] == '[A, B])
      passTrue @(L.RemoveAll '[A, B] '[A, B] == '[])
      passTrue @(L.RemoveAll '[A, C] '[A, B, C, D] == '[B, D])
      passTrue @(L.RemoveAll '[B, D] '[A, B, C, D] == '[A, C])
  describe "Concat" $ do
    it "should concat first with second list" $ do
      passTrue @(L.Concat '[] '[A, B] == '[A, B])
      passTrue @(L.Concat '[A, B] '[] == '[A, B])
      passTrue @(L.Concat '[A] '[B, C] == '[A, B, C])
      passTrue @(L.Concat '[A, B] '[C] == '[A, B, C])
      passTrue @(L.Concat '[A, B] '[C, D] == '[A, B, C, D])
  describe "Reverse" $ do
    it "should reverse a list" $ do
      passTrue @(L.Reverse '[] == '[])
      passTrue @(L.Reverse '[A] == '[A])
      passTrue @(L.Reverse '[A, B, C] == '[C, B, A])
  describe "Elem" $ do
    it "should return 'True when list contains type" $ do
      passTrue @(L.Elem A '[A] == 'True)
      passTrue @(L.Elem A '[B, A] == 'True)
    it "should return 'False when list does not contains type" $ do
      passTrue @(L.Elem A '[] == 'False)
      passTrue @(L.Elem A '[B] == 'False)
