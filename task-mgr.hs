
import Data.Time.Clock (getCurrentTime)

-- note > task > reminder
-- paragraph + action + timestamp

data Task = Task { description :: Description ,rules :: [Rule] }
data Description = { header :: String, body :: String }

-- let's model constraints somehow
-- we may want a DSL for the rules
--
-- rules operate on time objects, time ranges, days of week, hours, minutes, seconds, dates,
-- what are the "verbs" that operate on them?
-- nouns are like Tasks and description
-- is there a way to hash these entities?

-- list the tasks, and suggest one
main = do
    putStrLn "hello world"
    utc_time <- getCurrentTime
    putStr $ show utc_time
