module PlaygroundHaskus where

import qualified Haskus.Utils.Variant.Excepts as Exc

import qualified Haskus.Utils.Variant.Excepts.Syntax as S


import Haskus.Utils.Variant.Excepts
  ( Excepts(..),
    catchE,
    failureE,
    liftE, throwE, catchRemove, catchLiftLeft, runE
  )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Function ((&))
import Haskus.Utils.Types (Tail, Product)
import Haskus.Utils.Variant.VEither
    ( veitherProduct, pattern VLeft, pattern VRight, VEither, veitherLift, VEitherLift )


data ErrA = ErrA

data ErrB = ErrB

data ErrC = ErrC

data ErrD = ErrD


noErrors :: (Monad m) => a -> Excepts '[] m a
noErrors = pure

justError :: (Monad m) => e -> Excepts '[e] m a
justError = failureE

mapError :: (Monad m) => (e1 -> e) -> e1 -> Excepts '[e] m a
mapError f e = failureE (f e)

a :: Excepts '[ErrA] IO Int
a = pure 1

b :: Excepts '[ErrB] IO Int
b = pure 1

c :: Excepts '[ErrC] IO Int
c = pure 1

d :: Excepts '[ErrA, ErrD] IO Int
d = do
  _ <- liftE a
  _ <- catchE (mapError (\(_ :: ErrB) -> ErrD)) b
  _ <- catchE (\(_ :: ErrC) -> noErrors 2) c

  pure 1

appx :: Excepts '[ErrB, ErrA, ErrC, ErrD] IO b
appx = S.do
  _ <- b'
  _ <- c'
  S.liftIO $ print ("what"::String)
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

