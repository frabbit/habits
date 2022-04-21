module Test.PlaygroundHaskus where
import           Haskus.Utils.Variant.Excepts   ( Excepts
                                                , catchE
                                                , failureE
                                                , liftE
                                                )



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


d :: Excepts '[ErrA , ErrD] IO Int
d = do
  _ <- liftE a
  _ <- catchE (mapError (\(_ :: ErrB) -> ErrD)) b
  _ <- catchE (\(_ :: ErrC) -> noErrors 2) c

  pure 1
