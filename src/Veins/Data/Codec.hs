module Veins.Data.Codec where

import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Data.Validation (Validation(Failure, Success), bindValidation)

data ValidationErrorContext
  = VECField String
  | VECNamed String
  deriving (Show, Eq, Ord)

data ValidationError
  = VESimple String
  | VEContext ValidationErrorContext ValidationError
  | VEAll [ValidationError]
  deriving (Show,Eq,Ord)

instance Semigroup ValidationError where
  VEAll a <> VEAll b = VEAll (a <> b)
  a <> b = VEAll [a,b]


errSimple :: String -> ValidationError
errSimple = VESimple

errField :: String -> ValidationError -> ValidationError
errField s = VEContext (VECField s)

errNamed :: String -> ValidationError -> ValidationError
errNamed s = VEContext (VECNamed s)

errAll :: [ValidationError] -> ValidationError
errAll = VEAll

type Encoder from to = from -> Validation ValidationError to

type Decoder from to = from -> to

data Codec from to = Codec
  { encoder :: Encoder from to,
    decoder :: Decoder to from
  }

idCodec :: Codec a a
idCodec = Codec Success id

textFromString :: Codec String Text
textFromString =
  Codec
    { encoder = Success . Text.pack,
      decoder = Text.unpack
    }

mapValidationError :: (ValidationError -> ValidationError) -> Codec a b -> Codec a b
mapValidationError m c =
  Codec
    { encoder = \i -> case c.encoder i of
        Failure ve -> Failure $ m ve
        Success b -> Success b,
      decoder = c.decoder
    }

withContext :: ValidationErrorContext -> Codec a b -> Codec a b
withContext ctx = mapValidationError (VEContext ctx)


convError :: _ => Text -> Text -> _
convError a b s = VESimple $ "Cannot convert " <> show a <> " \"" <> show s <> "\" to " <> show b

encoderFromMaybe :: (a -> ValidationError) -> (a -> Maybe b) -> Encoder a b
encoderFromMaybe err conv s = Maybe.maybe (Failure (err s)) Success (conv s)

intFromString :: Codec String Int
intFromString =
  Codec
    { encoder = encoderFromMaybe (\s -> VESimple $ "Cannot convert String \"" <> s <> "\" to Int") readMaybe,
      decoder = show
    }


compose :: Codec a b -> Codec b c -> Codec a c
compose c1 c2 =
  Codec {encoder, decoder}
  where
    encoder i = c1.encoder i `bindValidation` c2.encoder
    decoder = c1.decoder . c2.decoder

encode :: Codec a b -> Encoder a b
encode c = c.encoder

decode :: Codec a b -> Decoder b a
decode c = c.decoder

withField :: String -> Codec a b -> Codec a b
withField name = withContext (VECField name)