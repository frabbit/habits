module Veins.Data.Time.Utils where
import Data.Time (UTCTime (UTCTime, utctDay), addDays, addUTCTime, secondsToNominalDiffTime)


addDaysToUTCTime :: Integer -> UTCTime -> UTCTime
addDaysToUTCTime days utime@UTCTime { utctDay } = utime { utctDay = addDays days utctDay }

addHoursToUTCTime :: Integer -> UTCTime -> UTCTime
addHoursToUTCTime hours =  addUTCTime (secondsToNominalDiffTime (1 * 60 * fromInteger hours))

addMillisecondsToUTCTime :: Integer -> UTCTime -> UTCTime
addMillisecondsToUTCTime milliseconds =  addUTCTime (secondsToNominalDiffTime (1 * fromInteger milliseconds))