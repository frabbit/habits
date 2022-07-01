module Habits.HabitsApp where

import Habits.Prelude
import Habits.Web.Server (ServerConfig(..), EmailServiceConfig (ESCMemory), mkApp)
import qualified Habits.Domain.RefreshTokenSecret as RTS
import qualified Habits.Domain.AccessTokenSecret as ATS
import Network.Wai.Handler.Warp (run)
import GHC.Natural (Natural, naturalToInteger)
import System.Environment (getEnv)
import Data.Text (pack)


mkConfig :: RTS.RefreshTokenSecret -> ATS.AccessTokenSecret -> ServerConfig
mkConfig rt at = ServerConfig {
  refreshTokenSecret = rt,
  accessTokenSecret = at,
  emailServiceConfig = ESCMemory
}

main :: Natural -> IO ()
main port = do
  rt <- RTS.mkRefreshTokenSecret . pack <$> getEnv "HABITS_REFRESH_TOKEN_SECRET"
  at <- ATS.mkAccessTokenSecret . pack <$> getEnv "HABITS_ACCESS_TOKEN_SECRET"
  let config = mkConfig rt at
  app <- mkApp config
  run (fromInteger . naturalToInteger $ port) app