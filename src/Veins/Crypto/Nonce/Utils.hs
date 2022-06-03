module Veins.Crypto.Nonce.Utils where

import Veins.Prelude
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import Test.QuickCheck (vector)

arbitraryNonce :: Gen Text
arbitraryNonce = do
  w :: [Word8] <- vector 18
  let res = B64URL.encode . BS.pack $ w
  pure $ Text.decodeUtf8 res