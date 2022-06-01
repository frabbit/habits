{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use let" #-}
module Haskus.Utils.Variant.Excepts.SyntaxSpec where

import Prelude
import Test.Hspec (Spec, describe, it, shouldBe, context)
import qualified Haskus.Utils.Variant.Excepts.Syntax as S
import Haskus.Utils.Variant.Excepts (evalE, Excepts, failureE, runE, pattern VRight)
import Utils (shouldBeIO)

import Haskus.Utils.Variant.VEither.Orphans ()
import Control.Monad.IO.Class (MonadIO(liftIO))


data ErrB = ErrB deriving (Show, Eq, Ord)
data ErrA = ErrA deriving (Show, Eq, Ord)
data ErrC = ErrC deriving (Show, Eq, Ord)
data ErrD = ErrD deriving (Show, Eq, Ord)

spec :: Spec
spec = describe "Syntax" $ do
  let
    withErrorsAB :: Excepts '[ErrA, ErrB] IO Int
    withErrorsAB = pure 1
    withErrorsCD :: Excepts '[ErrC, ErrD] IO Int
    withErrorsCD = pure 1
    noErrors :: Excepts '[] IO Int
    noErrors = pure 1

  context "S.pure should" $ do
    it "lift pure values into Except with an empty error list" $ do
      r <- evalE $ S.pure (1::Int)
      r `shouldBe` 1 :: IO ()
  context ">>= should" $ do
    it "combine errors with pure values" $ do
      let
        r :: Excepts '[ErrA,ErrB] IO Int
        r = S.do
          _ <- withErrorsAB
          noErrors
      runE r `shouldBeIO` VRight 1
    it "combine pure values with errors" $ do
      let
        r :: Excepts '[ErrA,ErrB] IO Int
        r = S.do
          _ <- noErrors
          withErrorsAB

      runE r `shouldBeIO` VRight 1
    it "combine pure values" $ do
      let
        r :: Excepts '[] IO Int
        r = S.do
          _ <- noErrors
          noErrors
      runE r `shouldBeIO` VRight 1
    it "combine computations with same errors" $ do
      let
        r :: Excepts '[ErrA, ErrB] IO Int
        r = S.do
          _ <- withErrorsAB
          withErrorsAB
      runE r `shouldBeIO` VRight 1
    it "combine computations with different errors" $ do
      let
        r :: Excepts '[ErrA,ErrB,ErrC,ErrD] IO Int
        r = S.do
          _ <- withErrorsAB
          withErrorsCD
      runE r `shouldBeIO` VRight 1
    it "combine 3 computations without errors" $ do
      let
        r :: Excepts '[] IO Int
        r = S.do
          _ <- noErrors
          _ <- noErrors
          noErrors

      runE r `shouldBeIO` VRight 1
    it "combine 3 computations having the same errors" $ do
      let
        r :: Excepts '[ErrA, ErrB] IO Int
        r = S.do
          _ <- withErrorsAB
          _ <- withErrorsAB
          withErrorsAB

      runE r `shouldBeIO` VRight 1
    it "combine 3 different computations" $ do
      let
        r :: Excepts '[ErrA, ErrB, ErrC, ErrD] IO Int
        r = S.do
          _ <- withErrorsAB
          _ <- withErrorsCD
          noErrors

      runE r `shouldBeIO` VRight 1


printM :: (MonadIO m) => String -> m ()
printM = liftIO . print

--appx :: Excepts '[ErrB, ErrA, ErrC, ErrD] IO b
appx :: Excepts '[ErrB, ErrA, ErrC, ErrD] IO b
appx = S.do
  _ <- b'
  _ <- c'
  _ <- S.coerce $ printM ("what"::String)
  _ <- d'
  _ <- a'
  e'
  where
    a' :: Excepts '[ErrA, ErrC] IO Int
    a' = pure 1
    b' :: Excepts '[ErrB, ErrA] IO Int
    b' = pure 1
    c' :: Excepts '[ErrC, ErrA, ErrB] IO Int
    c' = pure 1
    d' :: Excepts '[] IO Int
    d' = pure 1
    e' = failureE ErrD