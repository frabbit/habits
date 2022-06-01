{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Veins.Data.Type.Function where

import Prelude
import Data.Kind (Type)
import qualified Veins.Data.HList as HL
import qualified Veins.Data.Type.List as L


class MapReturn f r r' f' | f -> r, f r' -> f' where
  mapReturn :: f -> (r -> r') -> f'

instance {-# OVERLAPS #-} (MapReturn (b -> c) r r' f', r ~ ReturnOf (b -> c)) => MapReturn (a -> b -> c) r r' (a -> f') where
  mapReturn f m = \a -> mapReturn (f a) m

instance (IsFunction r ~ 'False, ReturnOf (a -> r) ~ r, f ~ (a -> r), f' ~ (a -> r')) => MapReturn f r r' f' where
  mapReturn f m = \a -> m (f a)

type family ReplaceReturn f r :: Type where
  ReplaceReturn (a -> b -> c) r = a -> ReplaceReturn (b -> c) r
  ReplaceReturn (a -> b) r = a -> r

type family Arguments f :: [Type] where
  Arguments (a -> b -> c) = (a ': Arguments (b -> c))
  Arguments (a -> b) = (a ': '[])

type ArgumentsRev f = L.Reverse (Arguments f)

type family ReturnOf f where
  ReturnOf (a -> b -> c) = ReturnOf (b -> c)
  ReturnOf (a -> r) = r

type CapturedConstraint f out =
  ( MapReturn
      ( ReplaceReturn
          f
          ( ReturnOf f,
            HL.HList (ArgumentsRev f)
          )
      )
      (ReturnOf f, HL.HList (ArgumentsRev f))
      (ReturnOf f, HL.HList (Arguments f))
      out,
    CapturedRev
      f
      ( ReplaceReturn
          f
          ( ReturnOf f,
            HL.HList
              (L.Reverse (Arguments f))
          )
      ),
    HL.HReverse (ArgumentsRev f) (Arguments f),
    out
      ~ ReplaceReturn
          f
          ( ReturnOf f,
            HL.HList (Arguments f)
          )
  )

class Captured f f' | f -> f' where
  captured :: f -> f'

instance
  {-# OVERLAPS #-}
  ( CapturedConstraint f out,
    f ~ (a -> b -> c)
  ) =>
  Captured (a -> b -> c) out
  where
  captured f = mapReturn (capturedRev f) (\(r, args) -> (r, HL.hreverse args))

instance
  ( CapturedConstraint f out,
    f ~ (a -> b)
  ) =>
  Captured (a -> b) out
  where
  captured f = mapReturn (capturedRev f) (\(r, args) -> (r, HL.hreverse args))

class CapturedRev f f' | f -> f' where
  capturedRev :: f -> f'

instance
  ( f ~ (a -> b),
    ReplaceReturn f (ReturnOf f, HL.HList (ArgumentsRev f)) ~ out,
    CapturedRev' f '[] out
  ) =>
  CapturedRev f out
  where
  capturedRev f = captured' f HL.hnil

class CapturedRev' f args f' | f args -> f' where
  captured' :: f -> HL.HList args -> f'

type family IsFunction f where
  IsFunction (a -> b) = 'True
  IsFunction _ = 'False

instance
  {-# OVERLAPS #-}
  (CapturedRev' (b -> c) (a ': args) out', (a -> out') ~ out) =>
  CapturedRev' (a -> b -> c) args out
  where
  captured' f args = \a -> captured' (f a) (HL.hcons a args)

instance (IsFunction b ~ 'False, f ~ (a -> b), f' ~ (a -> (b, HL.HList (a ': args)))) => CapturedRev' f args f' where
  captured' f args = \a -> (f a, HL.hcons a args)
