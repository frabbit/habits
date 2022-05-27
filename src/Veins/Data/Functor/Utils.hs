module Veins.Data.Functor.Utils where


(<.>) :: (Functor f) => (b -> c) -> (a -> f b) -> a -> f c
f <.> fa = fmap f . fa

infixr 9 <.>