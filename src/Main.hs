import Job
import Data.Aeson
import Data.Maybe
import Data.Map

main :: IO ()
main = printJobInfo $ fromJust (decode "{ \"class\": \"Email\", \"vars\": { \"to\": \"foo@example.com\", \"from\": \"steve@example.com\" } }" :: Maybe Job)

printJobInfo :: Job -> IO ()
printJobInfo job = do
  putStrLn $ "Class To Run: " ++ jobClass job
  mapM_ (putStrLn . displayOne) (toList $ getJobArgs $ jobVars job)
  where
    displayOne (k,v) = k ++ " : " ++ v


