
import Data.Time.Clock (getCurrentTime)


data Task = Task { description :: Description ,rules :: [Rule] }
data Description = { header :: String, body :: String }

main = do
    putStrLn "hello world"
    utc_time <- getCurrentTime
    putStr $ show utc_time
