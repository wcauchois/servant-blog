{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant.API
import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Time.Calendar
import qualified Data.ByteString.Char8 as B
import Database.PostgreSQL.Simple as P
import Database.PostgreSQL.Simple.FromRow
import System.IO
import Control.Monad
import GHC.Generics
import System.Environment
import Network.Wai
import Data.Aeson
import Network.Wai.Handler.Warp
import Servant

data User =
  User { name :: String
       , age :: Int
       } deriving (Eq, Show, Generic)

instance ToJSON User

instance FromRow User where
  fromRow = User <$> field <*> field

type UserAPI = "users" :> Get '[JSON] [User]

server :: P.Connection -> Server UserAPI
server conn = do
  (users :: [User]) <- liftIO $ query_ conn "select * from users"
  return users

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: P.Connection -> Application
app conn = serve userAPI (server conn)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (port :: Int) <- liftM read $ getEnv "PORT"
  putStrLn $ "Listening on port " ++ (show port)
  connString <- getEnv "CONNECTION_STRING"
  conn <- connectPostgreSQL $ B.pack connString
  run port (app conn)

