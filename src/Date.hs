module Date (dateStr) where

import Data.Time.Clock
import Data.Time.Calendar
import Data.List

dateStr :: IO String
dateStr = format <$> date

date :: IO (Year, MonthOfYear, DayOfMonth)
date = toGregorian . utctDay <$> getCurrentTime 

format :: (Year, MonthOfYear, DayOfMonth) -> String
format (year, month, day) = 
    intercalate "/"
    $ show
    <$> [day, month, fromIntegral year]
