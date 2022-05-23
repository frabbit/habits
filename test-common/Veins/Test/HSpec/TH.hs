module Veins.Test.HSpec.TH where
import Language.Haskell.TH (Exp (VarE), Pat, Q, Name)

import Test.Hspec.Expectations.Lifted (expectationFailure)

class ShouldMatchPattern a where
  shouldMatchPattern :: a -> Q Pat -> Q Exp

instance ShouldMatchPattern (Q Exp) where
  shouldMatchPattern :: Q Exp -> Q Pat -> Q Exp
  shouldMatchPattern e pat = [|
    case $e of
      $pat -> pure ();
      _ -> expectationFailure "Pattern does not match"
    |]
instance ShouldMatchPattern Name where
  shouldMatchPattern :: Name -> Q Pat -> Q Exp
  shouldMatchPattern n pat = [|
    case $e of
      $pat -> pure ();
      _ -> expectationFailure "Pattern does not match"
    |]
    where
      e = pure $ VarE n