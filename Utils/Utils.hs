module Utils.Utils where

import Prelude
import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Text

toString :: Day -> Text
toString day = dayText `append` ", " `append` month `append` " " `append` (pack $ show dayInt) `append` ", " `append` (pack $ show year)
   where (_,_,offset) = toWeekDate day
         dayText = pack $ show (toEnum (offset - 1) :: Weekday)
         month = pack $ show (toEnum (monthInt - 1) :: Month)
         (year, monthInt, dayInt) = toGregorian day

getDay :: IO Data.Time.Calendar.Day
getDay = fmap (localDay . zonedTimeToLocalTime) getZonedTime

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
   deriving (Enum, Show, Bounded)

data Month = January | February | March | April | May | June | July | August | September | October | November | December
   deriving (Enum, Show)


