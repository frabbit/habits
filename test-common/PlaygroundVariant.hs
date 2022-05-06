module PlaygroundVariant where

import Control.Monad.Except (ExceptT (..))
import Data.Variant
  ( CouldBe,
    CouldBeAnyOf,
    Variant,
    catchM,
    throwM,
  )

data ErrA = ErrA

data ErrB = ErrB

data ErrC = ErrC

data ErrD = ErrD

a :: (e `CouldBe` ErrA) => ExceptT (Variant e) IO Int
a = throwM ErrA

b :: (e `CouldBe` ErrB) => ExceptT (Variant e) IO Int
b = throwM ErrB

c :: (e `CouldBe` ErrC) => ExceptT (Variant e) IO Int
c = throwM ErrC

d :: (e `CouldBeAnyOf` '[ErrA, ErrC, ErrD]) => ExceptT (Variant e) IO Int
d = do
  _ <- a
  _ <- catchM b (\(_ :: ErrB) -> throwM ErrD)
  _ <- c

  pure 1
