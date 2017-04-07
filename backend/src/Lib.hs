{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors

data Scientist = Scientist
  { sId        :: Int
  , sFirstName :: String
  , sLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Scientist)

type API = "scientist" :> Get '[JSON] [Scientist]

startApp :: IO ()
startApp = do
  putStrLn "Starting"
  run 8080 app
  putStrLn "Finished"

app :: Application
app = simpleCors (serve api server)

api :: Proxy API
api = Proxy

server :: Server API
server = return scientists

scientists :: [Scientist]
scientists = [ Scientist 1 "Isaac" "Newton"
             , Scientist 2 "Albert" "Einstein"
             ]
