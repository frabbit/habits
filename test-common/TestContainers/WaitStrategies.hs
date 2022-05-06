module TestContainers.WaitStrategies where

import           Control.Applicative            ( Applicative(liftA2) )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( liftIO )
import           TestContainers.Docker          ( WaitUntilReady(..) )
import           UnliftIO.Async                 ( concurrently )

-- | @withExtraDelay n waitUntilReady@ waits first for the container to be ready and adds an additional @n@ seconds.
-- If the container is not ready by then a `TimeoutException` will be thrown.
--
-- @since 0.1.0.0
--
waitWithExtraDelay :: Int -> WaitUntilReady -> WaitUntilReady
waitWithExtraDelay milliSeconds (WaitUntilReady c) =
  WaitUntilReady $ \config container ->
    let (timeout', inner) = c config container
        wait              = do
          inner
          liftIO $ threadDelay (milliSeconds * 1000)
          pure ()
    in  (timeout', wait)

waitDelay :: Int -> WaitUntilReady
waitDelay milliSeconds = WaitUntilReady $ \_ _ ->
  let wait = do
        threadDelay (milliSeconds * 1000)
        pure ()
  in  (Nothing, liftIO wait)

waitPar :: WaitUntilReady -> WaitUntilReady -> WaitUntilReady
waitPar (WaitUntilReady first) (WaitUntilReady second) =
  WaitUntilReady $ \config container ->
    let (timeout1, inner1) = first config container
        (timeout2, inner2) = second config container
        wait               = do
          concurrently inner1 inner2
          pure ()
    in  (min timeout1 timeout2, wait)

waitSeq :: WaitUntilReady -> WaitUntilReady -> WaitUntilReady
waitSeq (WaitUntilReady first) (WaitUntilReady second) =
  WaitUntilReady $ \config container ->
    let (timeout1, inner1) = first config container
        (timeout2, inner2) = second config container
        wait               = do
          inner1
          inner2
          pure ()
    in  (min timeout1 timeout2, wait)
