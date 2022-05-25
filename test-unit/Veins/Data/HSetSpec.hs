{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Veins.Data.HSetSpec (spec) where

import Data.Function ((&))
import qualified Test.Hspec as HS
import Veins.Data.HSet
  ( Excluding,
    Intersection,
    IsSubset,
    empty,
    excluding,
    excludingSet,
    insert,
    intersection,
    remove,
    union,
  )
import Veins.Data.ToSymbol (ToSymbol)
import Veins.Data.Type.Bool (type (==))
import Veins.Test.HSpec (passTrue)

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

spec :: HS.Spec
spec = HS.describe "HSet.Alt" $ do
  HS.context "remove should" $ do
    HS.it "remove the type from the set" $ do
      remove @A (empty & insert A) `HS.shouldBe` empty
      remove @A (empty & insert B & insert A) `HS.shouldBe` (empty & insert B)
  HS.context "insert should" $ do
    HS.it "insert a value into the set" $ do
      show (empty & insert A) `HS.shouldBe` "A #: []"
    HS.it "replace an existing value when the same type is inserted" $ do
      (empty & insert 'a' & insert 'b') `HS.shouldBe` (empty & insert 'b')
  HS.context "union should" $ do
    HS.it
      "union two sets into one, elements from second set are taken when both contain the same type"
      $ do
        (empty `union` (empty & insert B)) `HS.shouldBe` (empty & insert B)
        ((empty & insert A) `union` empty) `HS.shouldBe` (empty & insert A)
        ((empty & insert A) `union` (empty & insert B))
          `HS.shouldBe` (empty & insert A & insert B)
        ((empty & insert 'a') `union` (empty & insert 'b'))
          `HS.shouldBe` (empty & insert 'b')
  HS.context "excludingSet should" $ do
    HS.it "excludes all elements from set 1 that exist in set 2" $ do
      let onlyA = empty & insert A
      (empty `excludingSet` empty) `HS.shouldBe` empty
      (onlyA `excludingSet` empty) `HS.shouldBe` onlyA
      (onlyA `excludingSet` onlyA) `HS.shouldBe` empty
      (onlyA `excludingSet` (empty & insert B & insert A)) `HS.shouldBe` empty
      (onlyA `excludingSet` (empty & insert B)) `HS.shouldBe` onlyA

  HS.context "excluding should" $ do
    HS.it
      "excludes all elements from set 1 which type is given as type application"
      $ do
        let onlyA = empty & insert A
        excluding @'[] empty `HS.shouldBe` empty
        excluding @'[] onlyA `HS.shouldBe` onlyA
        excluding @'[A] onlyA `HS.shouldBe` empty
        excluding @'[A, B] onlyA `HS.shouldBe` empty
        excluding @'[B] onlyA `HS.shouldBe` onlyA

  HS.context "IsSubset should" $ do
    HS.it "return 'True if the first set is a proper subset of the second" $ do
      passTrue @(IsSubset '[] '[] == 'True)
      passTrue @(IsSubset '[] '[A] == 'True)
      passTrue @(IsSubset '[A] '[] == 'False)
      passTrue @(IsSubset '[A, B] '[A] == 'False)
      passTrue @(IsSubset '[A, B] '[A, B] == 'True)
      passTrue @(IsSubset '[A] '[A, C] == 'True)
  HS.context "Excluding should" $ do
    HS.it "exclude all types that are part of the second set from the first set" $ do
      passTrue @(Excluding '[] '[] == '[])
      passTrue @(Excluding '[] '[A] == '[])
      passTrue @(Excluding '[A] '[] == '[A])
      passTrue @(Excluding '[A, B] '[A] == '[B])
      passTrue @(Excluding '[A, B] '[A, B] == '[])
      passTrue @(Excluding '[A] '[B, C] == '[A])
  HS.context "Intersection should" $ do
    HS.it "drop all distinct elements from both sets" $ do
      passTrue @(Intersection '[] '[] == '[])
      passTrue @(Intersection '[] '[A] == '[])
      passTrue @(Intersection '[A] '[] == '[])
      passTrue @(Intersection '[A, B] '[A] == '[A])
      passTrue @(Intersection '[A, B] '[A, B] == '[A, B])
      passTrue @(Intersection '[A, B] '[A, C] == '[A])
      passTrue @(Intersection '[A, C] '[B, C] == '[C])
      passTrue @(Intersection '[A] '[B, C] == '[])

  HS.context "intersection should" $ do
    HS.it
      "excludes all elements from set 1 which are not given in type application"
      $ do
        let onlyA = empty & insert A
        intersection @'[] empty `HS.shouldBe` empty
        intersection @'[] onlyA `HS.shouldBe` empty
        intersection @'[A] onlyA `HS.shouldBe` onlyA
        intersection @'[A, B] onlyA `HS.shouldBe` onlyA
        intersection @'[B] onlyA `HS.shouldBe` empty
