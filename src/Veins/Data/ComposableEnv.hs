{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use fewer LANGUAGE pragmas" #-}

module Veins.Data.ComposableEnv
  ( ReaderCE,
    LayerCE,
    empty,
    remove,
    addLayer,
    provideAndChainLayer,
    insert,
    ComposableEnv,
    MkSorted,
    expandEnv,
    chainFromEnv,
    expandEnvBy,
    union,
    provideAll',
    provideAll,
    expandLayer,
    get,
    provideLayer,
    provideLayerFlipped,
    provideLayer',
    (>>=),
    fail,
    return,
    pure,
    provideAndChainLayerFlipped
  , (<<-&&), (<<-))
where

import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import Data.Kind (Type)
import qualified Veins.Data.HList as HL
import qualified Veins.Data.HSet as H
import qualified Veins.Data.HSortedList as HSL
import qualified Veins.Data.Has as Has
import Veins.Data.ToSymbol (CmpToSymbol)
import Prelude
  ( Eq (..),
    Monad,
    Show (show),
    ($),
    (.),
  )
import qualified Prelude as P
import qualified Veins.Data.Type.List as L

type ReaderCE env m x = ReaderT (ComposableEnv env) m x

type LayerCE env m out = ReaderCE  env m (ComposableEnv out)

type family MkSorted (xs :: [Type]) where
  MkSorted xs = ComposableEnv (Sorted xs)

type family ConvertAcc (tup::(k, [k], [k])) :: (P.Maybe k, [k]) where
  ConvertAcc '(x, _, acc) = '( 'P.Just x, L.Reverse acc)

type family FindSmallest (xs :: [k]) :: (P.Maybe k, [k]) where
  FindSmallest '[] = '( 'P.Nothing, '[])
  FindSmallest (x ': tail) = ConvertAcc (FindSmallest' x tail '[])


type family FindSmallest' (c::k) (xs :: [k]) (acc:: [k]) :: (k, [k], [k]) where
  FindSmallest' min (x ': tail) acc = FindSmallestCase (CmpToSymbol min x) min x tail acc
  FindSmallest min '[] acc= '(min, '[], acc)

type family FindSmallestCase (ord :: P.Ordering) min y (tail :: [k]) (acc :: [k]):: (k, [k], [k]) where
  FindSmallestCase 'P.EQ x y tail acc = FindSmallest' x tail (y ': acc)
  FindSmallestCase 'P.LT x y tail acc = FindSmallest' x tail (y ': acc)
  FindSmallestCase 'P.GT x y tail acc = FindSmallest' y tail (x ': acc)

type family Sorted (xs :: [k]) :: [k] where
  Sorted '[] = '[]
  Sorted (x : '[]) = x : '[]
  Sorted (x : tail) = Sorted' x (FindSmallest tail)

type family Sorted' (x::k) (rest:: ( P.Maybe k, [k])) :: [k] where
  Sorted x '( 'P.Nothing, _ )= x : '[]
  Sorted x '( 'P.Just y, tail) = SortedCase (CmpToSymbol x y) x y tail

type family SortedCase (ord :: P.Ordering) x y (tail :: [k]) :: [k] where
  SortedCase 'P.EQ x y tail = (x : Sorted (y : tail))
  SortedCase 'P.LT x y tail = (x : Sorted (y : tail))
  SortedCase 'P.GT x y tail = (y : Sorted (x : tail))

newtype ComposableEnv e = ComposableEnv (H.HSet e)

instance (Show (H.HSet x)) => Show (ComposableEnv x) where
  show = show . unwrap

instance (Eq (H.HSet x)) => Eq (ComposableEnv x) where
  a == b = unwrap a == unwrap b

unwrap :: forall e. ComposableEnv e -> H.HSet e
unwrap (ComposableEnv e) = e

wrap :: forall e. H.HSet e -> ComposableEnv e
wrap = ComposableEnv

empty :: ComposableEnv '[]
empty = ComposableEnv H.empty

remove ::
  forall x xs.
  (H.CRemove x xs (HSL.Remove x xs)) =>
  ComposableEnv xs ->
  ComposableEnv (HSL.Remove x xs)
remove = ComposableEnv . H.remove @x . unwrap

insert ::
  (H.CInsert x xs (HSL.Insert x xs)) =>
  x ->
  ComposableEnv xs ->
  ComposableEnv (HSL.Insert x xs)
insert x = ComposableEnv . H.insert x . unwrap

get :: (HL.HGetFirst x xs) => ComposableEnv xs -> x
get = H.get . unwrap

union ::
  forall xs ys.
  (H.CUnion xs ys (H.Union xs ys)) =>
  ComposableEnv xs ->
  ComposableEnv ys ->
  ComposableEnv (H.Union xs ys)
union xs ys = ComposableEnv $ H.union (unwrap xs) (unwrap ys)

intersection ::
  forall ys xs.
  (H.CIntersection ys xs (H.Intersection xs ys)) =>
  ComposableEnv xs ->
  ComposableEnv (H.Intersection xs ys)
intersection xs = ComposableEnv $ H.intersection @ys (unwrap xs)

provideAll' ::
  forall e1 e2 r out e2'.
  ( H.IntersectionC e2 e1 e2',
    H.Excluding e1 e2' ~ out,
    H.Union out e2' ~ e1,
    H.CUnion out e2' e1
  ) =>
  (ComposableEnv e1 -> r) ->
  ComposableEnv e2 ->
  ComposableEnv out ->
  r
provideAll' f e2 = \(eout :: ComposableEnv (H.Excluding e1 e2')) -> f (eout `union` intersection @e1 e2)

provideAll ::
  forall e1 e2 e2' out r m.
  ( H.IntersectionC e2 e1 e2',
    H.Excluding e1 e2' ~ out,
    H.Union out e2' ~ e1,
    H.CUnion out e2' e1
  ) =>
  ReaderCE e1 m r ->
  ComposableEnv e2 ->
  ReaderCE out m r
provideAll f e = ReaderT $ provideAll' (runReaderT f) e

lift ::
  forall e0 e1 m a.
  (H.CExcluding (H.Difference e1 e0) e0 e1) =>
  ReaderCE e1 m a ->
  ReaderCE e0 m a
lift r = ReaderT $ \(e :: ComposableEnv e0) ->
  runReaderT r (wrap (H.excluding @(H.Difference e1 e0) (unwrap e)))

type Bind =
  forall e1 e2 m a b.
  ( Monad m,
    H.CExcluding (H.Difference e1 (H.Union e1 e2)) (H.Union e1 e2) e1,
    H.CExcluding (H.Difference e2 (H.Union e1 e2)) (H.Union e1 e2) e2
  ) =>
  ReaderCE e1 m a ->
  (a -> ReaderCE e2 m b) ->
  ReaderCE (H.Union e1 e2) m b

bind :: Bind
bind r f = do
  a <- lift r
  lift (f a)

provideLayerFlipped :: _ => _
provideLayerFlipped = P.flip provideLayer

provideLayer ::
  forall e0 e1 e2 m r out e2'.
  ( Monad m,
    H.Excluding e2 e1 ~ e2', --
    H.Union e2' e0 ~ out,
    _
  ) =>
  LayerCE e0 m e1 ->
  ReaderCE e2 m r ->
  ReaderCE out m r
provideLayer layer c =
  let f :: forall. ComposableEnv (H.Union e2' e0) -> m r
      f env = do
        let e0 :: ComposableEnv e0
            e0 = intersection @e0 env
        l <- runReaderT layer e0
        let e2 :: forall. ComposableEnv e2
            e2 = intersection @e2 env `union` l
        runReaderT c e2
   in ReaderT f

provideLayer' ::
  forall e0 e1 e2 m r out e2' e1'.
  ( Monad m,
    H.IntersectionC e1 e2 e1',
    H.Excluding e2 e1' ~ e2',
    H.CExcluding e0 out (H.Excluding out e0),
    H.Union e2' e0 ~ out,
    H.UnionC (H.Excluding out e0) e1' e2,
    H.ExcludingC out e2' e0
  ) =>
  LayerCE e0 m e1 ->
  ReaderCE e2 m r ->
  ReaderCE out m r
provideLayer' layer c =
  let f :: forall. ComposableEnv (H.Union e2' e0) -> m r
      f env = do
        let e1 :: ComposableEnv e0
            e1 = excluding @(H.Excluding e2 e1') env
        l <- runReaderT layer e1
        let (l' :: ComposableEnv e1') = intersection @e2 l
            e2 :: forall. ComposableEnv e2
            e2 = excluding @e0 env `union` l'
        runReaderT c e2
   in ReaderT f

expandEnv ::
  forall e2 e1 m a.
  ( H.CIntersection e1 e2 e1,
    H.Excluding e1 e1 ~ '[],
    H.Intersection e2 e1 ~ e1
  ) =>
  ReaderCE e1 m a ->
  ReaderCE e2 m a
expandEnv or = ReaderT f
  where
    f efull = runReaderT (provideAll or efull) empty

expandEnvBy ::
  forall e0 e1 m a.
  ( H.CIntersection e1 (H.Union e0 e1) e1,
    H.Excluding e1 e1 ~ '[],
    H.Intersection (H.Union e0 e1) e1 ~ e1
  ) =>
  ReaderCE e1 m a ->
  ReaderCE (H.Union e0 e1) m a
expandEnvBy or = ReaderT f
  where
    f efull = runReaderT (provideAll or efull) empty

chainFromEnv ::
  forall e2 e0 e1 m.
  ( Monad m,
    H.CUnion
      e1
      (H.Intersection e0 e2)
      (H.Union e1 (H.Intersection e0 e2)),
    H.CIntersection e2 e0 (H.Intersection e0 e2),
    H.Union e1 (H.Intersection e0 e2) ~ H.Union e1 e2
  ) =>
  LayerCE e0 m e1 ->
  LayerCE e0 m (H.Union e1 e2)
chainFromEnv (ReaderT f) = ReaderT $ \e -> do
  base <- (f e :: m (ComposableEnv e1))
  let r = base `union` intersection @e2 e
  P.pure r

expandLayer ::
  forall e0 ein eout m.
  ( Monad m,
    H.CUnion
      eout
      (H.Intersection (H.Union e0 ein) e0)
      (H.Union eout (H.Intersection (H.Union e0 ein) e0)),
    H.CIntersection
      e0
      (H.Union e0 ein)
      (H.Intersection (H.Union e0 ein) e0),
    H.CIntersection ein (H.Union e0 ein) ein,
    H.Excluding ein ein ~ '[],
    H.Union eout (H.Intersection (H.Union e0 ein) e0)
      ~ H.Union eout e0,
    H.Union ein e0 ~ H.Union e0 ein,
    H.Intersection (H.Union e0 ein) ein ~ ein
  ) =>
  LayerCE ein m eout ->
  LayerCE (H.Union ein e0) m (H.Union eout e0)
expandLayer = chainFromEnv @e0 . expandEnvBy @e0

addLayer ::
  ( Monad m,
    H.CExcluding
      (H.Difference e2 (H.Union e2 (H.Union e3 '[])))
      (H.Union e2 (H.Union e3 '[]))
      e2,
    H.CExcluding
      (H.Difference (H.Union e3 '[]) (H.Union e2 (H.Union e3 '[])))
      (H.Union e2 (H.Union e3 '[]))
      (H.Union e3 '[]),
    H.CExcluding
      (H.Difference e3 (H.Union e3 '[]))
      (H.Union e3 '[])
      e3,
    H.CExcluding
      (H.Difference '[] (H.Union e3 '[]))
      (H.Union e3 '[])
      '[],
    H.CUnion xs ys (H.Union xs ys)
  ) =>
  LayerCE e2 m xs ->
  LayerCE e3 m ys ->
  LayerCE
    (H.Union e2 (H.Union e3 '[]))
    m
    (H.Union xs ys)
addLayer p0 p1 =
  p0 >>= \l1 ->
    p1 >>= \l2 ->
      let l0 = l1 `union` l2
       in pure l0

(>>=) :: Bind
(>>=) = bind

type Pure = forall m a. (Monad m) => a -> ReaderCE '[] m a

pure :: Pure
pure a = ReaderT $ \_ -> P.pure a

return :: Pure
return = pure

fail :: P.MonadFail m => P.String -> ReaderCE '[] m a
fail = P.fail

excluding :: forall e0 e1. (H.CExcluding e0 e1 (H.Excluding e1 e0)) => ComposableEnv e1 -> ComposableEnv (H.Excluding e1 e0)
excluding = wrap . H.excluding @e0 . unwrap

provideAndChainLayer ::
  forall e0 e1 o1 o2 m.
  ( Monad m,
    _
  ) =>
  LayerCE e0 m o1 ->
  LayerCE e1 m o2 ->
  LayerCE (H.Union (H.Excluding (H.Union o1 e1) o1) e0) m (H.Union o2 o1)
provideAndChainLayer layer = provideLayer layer . chainFromEnv @o1 . expandEnvBy @o1

provideAndChainLayerFlipped ::
  forall e0 e1 o1 o2 m.
  ( Monad m,
    _
  ) =>
  LayerCE e1 m o2 ->
  LayerCE e0 m o1 ->
  LayerCE (H.Union (H.Excluding (H.Union o1 e1) o1) e0) m (H.Union o2 o1)
provideAndChainLayerFlipped a b = provideAndChainLayer b a


-- | A combinator for layers. The resulting environment of the second layer is provided for the first layer.
-- | The union of the remaining environment of the first layer and the environment of the second layer become the new environment.
(<<-&&) ::
  forall e0 e1 o1 o2 m.
  ( Monad m,
    _
  ) =>
  LayerCE e1 m o2 ->
  LayerCE e0 m o1 ->
  LayerCE (H.Union (H.Excluding (H.Union o1 e1) o1) e0) m (H.Union o2 o1)
(<<-&&) = provideAndChainLayerFlipped

(<<-) :: _ => _
(<<-) = provideLayerFlipped

infixl 9 <<-&&
infixl 9 <<-

instance (HL.HGetFirst y m) => Has.Has y (ComposableEnv m) where
  get = get