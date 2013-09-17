module Job where

-- Parses something that looks like this:
-- {
--     "class": "Email",
--     "vars": {
--       "to": "foo@example.com",
--       "from": "steve@example.com"
--     }
-- }
--
-- Into something like this: 
--   Just (Job {
--          jobClass = "Email",
--          jobVars = JobArgs (fromList [("from","steve@example.com"),("to","foo@example.com")])}
--        )

import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Map

---------------------------------
newtype JobArgs = JobArgs { getJobArgs :: (Map String String) }
  deriving Show

instance FromJSON JobArgs where
  parseJSON val = JobArgs <$> parseJSON val
---------------------------------

data Job = Job { jobClass :: String
               , jobVars  :: JobArgs }
               deriving Show

instance FromJSON Job where
  parseJSON (Object o) = do
    let klass = o .: "class"
    let vars  = o .: "vars" >>= parseJSON
    Job <$> klass <*> vars
  parseJSON _          = mzero

