{-# OPTIONS_GHC -Wno-orphans #-}
module Haskus.Utils.Variant.VEither.Orphans where
import Haskus.Utils.Variant (V)
import Haskus.Utils.Variant.VEither (VEither, pattern VLeft, pattern VRight)

instance ( Eq a, Eq (V es)) => Eq (VEither es a) where
  (==) (VLeft as) (VLeft bs) =  as == bs
  (==) (VRight a) (VRight b) = a == b
  (==) _ _ = False