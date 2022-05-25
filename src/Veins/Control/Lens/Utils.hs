module Veins.Control.Lens.Utils where

import Control.Lens (makeLensesWith, lensRules, lensField, (.~), (&), DefName (TopName))
import Language.Haskell.TH (nameBase, mkName, Name, Dec, Q)

makeLensesWithSuffixL :: Name -> Q [Dec]
makeLensesWithSuffixL = makeLensesWithSuffix "L"

makeLensesWithSuffix :: String -> Name -> Q [Dec]
makeLensesWithSuffix suffix = makeLensesWith $ lensRules
  & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name ++ suffix)])
