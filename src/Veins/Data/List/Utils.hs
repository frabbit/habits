module Veins.Data.List.Utils where

safeHead :: [a] -> Maybe a
safeHead (a : _) = Just a
safeHead [] = Nothing