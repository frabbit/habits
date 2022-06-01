module Veins.Test.HSpec.TH where

import Language.Haskell.TH (Exp (LitE, VarE), Lit (StringL), Name, Pat, Q, pprint)
import Test.Hspec.Expectations.Lifted (expectationFailure)

class ShouldMatchPattern a where
  shouldMatchPattern :: a -> Q Pat -> Q Exp

patToExp :: Pat -> Exp
patToExp = LitE . StringL . pprint

instance ShouldMatchPattern (Q Exp) where
  shouldMatchPattern :: Q Exp -> Q Pat -> Q Exp
  shouldMatchPattern e pat = do
    let p = patToExp <$> pat
    [|
      case $e of
        $pat -> pure ()
        v -> expectationFailure ("Expected pattern: " <> $p <> "\nActual value: " <> show v)
      |]

instance ShouldMatchPattern Name where
  shouldMatchPattern :: Name -> Q Pat -> Q Exp
  shouldMatchPattern n = shouldMatchPattern (pure $ VarE n :: Q Exp)