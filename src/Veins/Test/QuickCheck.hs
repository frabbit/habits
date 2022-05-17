module Veins.Test.QuickCheck where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (Gen)
import Test.QuickCheck.Utf8 (genValidUtf8)

genValidUtf8WithoutNullByte :: Gen Text
genValidUtf8WithoutNullByte = do
  s <- genValidUtf8
  if '\NUL' `T.elem` s then genValidUtf8WithoutNullByte else pure s