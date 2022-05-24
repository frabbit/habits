{-# LANGUAGE PartialTypeSignatures #-}

module Veins.Data.CodecSpec where

import Data.Text (Text)
import Data.Validation (Validation (Failure, Success))
import qualified Test.Hspec as HS
import Veins.Data.Codec (Codec)
import qualified Veins.Data.Codec as Codec

data Gender = Female | Male | Diverse deriving (Show, Eq)

genderFromString :: String -> Maybe Gender
genderFromString = \case
  "f" -> Just Female
  "m" -> Just Male
  "d" -> Just Diverse
  _ -> Nothing

genderToString :: Gender -> String
genderToString = \case
  Female -> "f"
  Male -> "m"
  Diverse -> "d"

data PersonDto = PersonDto {age :: String, name :: String, gender :: String} deriving (Show, Eq)

data Person = Person {age :: Int, name :: Text, gender :: Gender} deriving (Show, Eq)

personFromPersonDto :: Codec PersonDto Person
personFromPersonDto = Codec.withContext (Codec.VECNamed "PersonFromPersonDto") (Codec.Codec encoder decoder)
  where
    ageC = Codec.withField "age" Codec.intFromString
    nameC = Codec.withField "name" Codec.textFromString
    genderC = Codec.withField "gender" $ Codec.Codec (Codec.encoderFromMaybe (\e -> Codec.VESimple $ "Cannot convert String \"" <> e <> "\" to Gender") genderFromString) genderToString
    encoder p = Person <$> Codec.encode ageC p.age <*> Codec.encode nameC p.name <*> Codec.encode genderC p.gender
    decoder p = PersonDto (Codec.decode ageC p.age) (Codec.decode nameC p.name) (Codec.decode genderC p.gender)

spec :: HS.Spec
spec = HS.describe "Codec" $ do
  HS.context "decode should"  $ do
    HS.it "always be successful" $ do
      let p = Person {age = 27, name = "Jane", gender = Female}
      Codec.decode personFromPersonDto p `HS.shouldBe` PersonDto "27" "Jane" "f"
  HS.context "encode should" $ do
    HS.it "succeed when data is valid" $ do
      let p = PersonDto "27" "Jane" "f"
      Codec.encode personFromPersonDto p `HS.shouldBe` (Success $ Person {age = 27, name = "Jane", gender = Female})
    HS.it "fail with a proper error when data is invalid" $
      do
        let p = PersonDto "27" "Jane" "whatever"
            valError =
              Codec.errNamed
                "PersonFromPersonDto"
                (Codec.errField "gender" $ Codec.errSimple "Cannot convert String \"whatever\" to Gender")

        Codec.encode personFromPersonDto p `HS.shouldBe` Failure valError
    HS.it "accumulate multiple errors" $
      do
        let p = PersonDto "invalidage" "Jane" "whatever"
            valError =
              Codec.errNamed "PersonFromPersonDto" $
                Codec.errAll
                  [ Codec.errField "age" $ Codec.errSimple "Cannot convert String \"invalidage\" to Int",
                    Codec.errField "gender" $ Codec.errSimple "Cannot convert String \"whatever\" to Gender"
                  ]
        Codec.encode personFromPersonDto p `HS.shouldBe` Failure valError
