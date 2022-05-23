module Veins.Test.HSpec.TH where
import Language.Haskell.TH (Exp (VarE), Pat, Q, Name)

import Test.Hspec.Expectations.Lifted (expectationFailure)

shouldMatchPattern :: Q Exp -> Q Pat -> Q Exp
shouldMatchPattern e pat = [|
  case $e of
    $pat -> pure ();
    _ -> expectationFailure "Pattern does not match"
  |]

class ShouldMatchPattern a where
  shouldMatchPattern1 :: a -> Q Pat -> Q Exp

instance ShouldMatchPattern (Q Exp) where
  shouldMatchPattern1 :: Q Exp -> Q Pat -> Q Exp
  shouldMatchPattern1 e pat = [|
    case $e of
      $pat -> pure ();
      _ -> expectationFailure "Pattern does not match"
    |]
instance ShouldMatchPattern Name where
  shouldMatchPattern1 :: Name -> Q Pat -> Q Exp
  shouldMatchPattern1 n pat = [|
    case $e of
      $pat -> pure ();
      _ -> expectationFailure "Pattern does not match"
    |]
    where
      e = pure $ VarE n