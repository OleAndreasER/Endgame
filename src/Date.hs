module Date (dateStr) where

import Data.Time.Clock ( UTCTime(utctDay), getCurrentTime )
import Data.Time.Calendar
    ( Year, MonthOfYear, DayOfMonth, toGregorian )
import Data.List ( intercalate )

dateStr :: IO String
dateStr = format <$> date

date :: IO (Year, MonthOfYear, DayOfMonth)
date = toGregorian . utctDay <$> getCurrentTime 

format :: (Year, MonthOfYear, DayOfMonth) -> String
format (year, month, day) = 
    intercalate "/"
    $ show
    <$> [day, month, fromIntegral year]
