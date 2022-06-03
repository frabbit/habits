module Veins.Test.Prelude (
  module M
) where

import Veins.Prelude as M

import Veins.Test.HSpec.TH as M (shouldMatchPattern)

import Prelude as M hiding (id)

import Veins.Test.Mock as M (getSpyArgsIO, mockReturn, mockify, withSpy)

import Test.Hspec as M (Spec, fdescribe, it, describe, fit, xit, xdescribe)

import Test.QuickCheck as M (property)

import Veins.Test.QuickCheck as M (propertyOne)
