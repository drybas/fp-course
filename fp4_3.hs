
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
--import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry =  LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String} 

logLevelToString :: LogLevel -> String
logLevelToString x = case x of  
                       Error -> "Error" 
                       Warning -> "Warning" 
                       Info -> "Info" 

-- let x = LogEntry (UTCTime (fromGregorian 2021 12 25) (secondsToDiffTime 0)) Error "Cannot find file"

logEntryToString :: LogEntry -> String
logEntryToString e = timeToString (timestamp e) ++ ": " ++ logLevelToString (logLevel e) ++ ": " ++ (message e)

----------------------------------------------------------------

data Person = Person { firstName :: String, lastName :: String, age :: Int } 
        deriving(Show)

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 { lastName = lastName p1 }

abbrFirstName :: Person -> Person
abbrFirstName p = case p of
        (Person { firstName = x:_:_}) -> p { firstName = [x] ++ "." }
        _ -> p

--abbrFirstName p@(Person {firstName = x:_:_}) = p {firstName = x:"."}
--abbrFirstName p = p

--------------------------------------------------------
data Shape = Circle Double | Rectangle Double Double

isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False


