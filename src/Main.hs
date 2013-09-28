import Job
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as SBS
import Control.Monad (forever)

import Database.Redis hiding (decode)

type Queue = SBS.ByteString
type JobHandler = Queue -> JobArgs -> IO ()

data WorkerConfig = WorkerConfig { 
                         name         :: String
                       , queues       :: [(Queue, JobHandler)] -- | Process all the messages on the first queue before the next
                       , numberToFork :: Int
                       }

instance Show WorkerConfig where
  show WorkerConfig {name = name, queues = queues, numberToFork = numberToFork} = 
    "WorkerConfig {name = \"" ++ name ++ "\", queues = " ++ unwords (map (SBS.unpack . fst) queues) ++ ", numberToFork = " ++ show numberToFork ++ "}"

data ResqueConfig = ResqueConfig {
                         host           :: String
                       , port           :: Int
                       , databaseNumber :: Int
                       , password       :: Maybe BS.ByteString
                       , prefix         :: String
                       } deriving (Show)

defaultResqueConfig :: ResqueConfig
defaultResqueConfig = ResqueConfig { host = "localhost", port = 1000, databaseNumber = 1, password = Nothing, prefix = "resque:" }

main :: IO ()
main = do
  let resqueConfig = defaultResqueConfig
  forkResqueWorker resqueConfig controlEventWorkerConfig

controlEventWorkerConfig :: WorkerConfig
controlEventWorkerConfig = WorkerConfig {
                             name = "ControlEventWorker"
                           , queues = [("ControlEvent", controlEventHandler)]
                           , numberToFork = 5
                           }

controlEventHandler :: JobHandler
controlEventHandler _ jobConfig = putStrLn "Called me!"


forkResqueWorker :: ResqueConfig -> WorkerConfig -> IO ()
forkResqueWorker resqueConfig workerConfig = do
  redisConn <- connect (resqueConfigToRedisConfig resqueConfig)

  -- forever $ do
  maybeJob <- getJobFromQueues redisConn (fst . head $ queues workerConfig)

  putStrLn "Got this from redis:"
  print maybeJob

  case maybeJob of
    Nothing  -> putStrLn "No Job Found"
    Just job -> do
      let parsedJob = parseJob job
      putStrLn "Got parsed job: "
      print parsedJob
      runFunction


-- Run some redis magic, pull the result back into IO monad
getJobFromQueues :: Connection -> Queue -> IO (Maybe ByteString)
getJobFromQueues conn queueName = do
  answer <- getRedisAnswer
  case answer of
    Just x  -> return $ Just $ BS.fromChunks [x]
    Nothing -> return Nothing
  where
  getRedisAnswer = runRedis conn $ do
                     maybeResult <- lpop queueName
                     return $ case maybeResult of
                       Left _  -> Nothing
                       Right x -> x

  -- "{ \"class\": \"Email\", \"vars\": { \"to\": \"foo@example.com\", \"from\": \"steve@example.com\" } }"

-- Turn a bytestring into a parsed job object
parseJob :: ByteString -> Job
parseJob jobJson = fromJust (decode jobJson :: Maybe Job)

-- And finally run the function that's appropriate
runFunction :: IO ()
runFunction = return ()

resqueConfigToRedisConfig :: ResqueConfig -> ConnectInfo
resqueConfigToRedisConfig ResqueConfig { host = host, port = port, databaseNumber = databaseNumber, password = password, prefix = prefix } =
      defaultConnectInfo {
        connectHost           = host
      {- , connectPort           = PortNum port -}
      {- , connectAuth           = password -}
      , connectMaxConnections = 10
      }

