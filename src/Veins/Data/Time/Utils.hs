module Veins.Data.Time.Utils where
import Data.Time (UTCTime (UTCTime, utctDay), addDays)

addDaysToUTCTime :: Integer -> UTCTime -> UTCTime
addDaysToUTCTime days utime@UTCTime { utctDay } = utime { utctDay = addDays days utctDay }