module Data.Time.Util where
import           Data.Time.Clock
import           Data.Time.Format

timeDateTime :: IO String
timeDateTime = timeF "%F %H:%M.%S"

timeF :: String -> IO String
timeF format = fmap (formatTime defaultTimeLocale format) getCurrentTime
