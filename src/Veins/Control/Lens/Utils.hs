module Veins.Control.Lens.Utils where

import Prelude

import Control.Lens (makeLensesWith, lensRules, lensField, (.~), (&), DefName (TopName))
import Language.Haskell.TH (nameBase, mkName, Name, Dec, Q)

makeLensesWithSuffixL :: Name -> Q [Dec]
makeLensesWithSuffixL = makeLensesWithSuffix "L"

makeLensesWithSuffix :: String -> Name -> Q [Dec]
makeLensesWithSuffix suffix = makeLensesWithNameMap (<> suffix)

makeLensesWithNameMap :: (String -> String) -> Name -> Q [Dec]
makeLensesWithNameMap nameMap = makeLensesWith $ lensRules
  & lensField .~ (\_ _ name -> [TopName (mkName $ nameMap . nameBase $ name)])


makeLensesWithoutUnderscoreAndWithSuffixL :: Name -> Q [Dec]
makeLensesWithoutUnderscoreAndWithSuffixL = makeLensesWithNameMap $ \s -> dropFirst s <> "L"
  where
    dropFirst ('_' : xs) = xs
    dropFirst x = x
