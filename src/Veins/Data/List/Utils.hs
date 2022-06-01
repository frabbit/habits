module Veins.Data.List.Utils where

import Prelude

safeHead :: [a] -> Maybe a
safeHead (a : _) = Just a
safeHead [] = Nothing