{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.FileEmbed
import Destiny.Model
import Network.HTTP.Types
import Network.Mime
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Safe.Foldable
import System.FilePath
import Text.Printf

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text

data State = State
    { stateWorld :: World
    , stateClients :: [Client]
    }

data Client = Client
    { clientId :: Id
    , clientConnection :: Connection
    }

main :: IO ()
main = do
    stateVar <- newMVar emptyState
    putStrLn $ printf "Listening on port %d." $ getPort defaultSettings
    runSettings defaultSettings $
        websocketsOr defaultConnectionOptions (webSocketsApp stateVar) httpApp

emptyState :: State
emptyState = State
    { stateWorld = World
        { worldEntities = []
        , worldLastRoll = 0
        }
    , stateClients = []
    }

clientAppDir :: [(FilePath, ByteString)]
clientAppDir = $(embedDir $ ".." </> "client" </> "app")

httpApp :: Application
httpApp request respond = respond $ case rawPathInfo request of
    "/" -> response "index.html"
    path -> response $ ByteString.unpack $ ByteString.tail path
  where
    response path = case lookup path clientAppDir of
        Just content ->
            let contentType = defaultMimeLookup $ Text.pack path
            in responseBuilder status200 [("Content-Type", contentType)] $ byteString content
        Nothing -> responseLBS status404 [("Content-Type", "text/plain")] "404 Not Found"

webSocketsApp :: MVar State -> ServerApp
webSocketsApp stateVar pending = do
    connection <- acceptRequest pending
    client <- modifyMVar stateVar $ return . addClient connection
    finally
        (forever $ handleMessage connection stateVar)
        (modifyMVar_ stateVar $ return . removeClient (clientId client))

addClient :: Connection -> State -> (State, Client)
addClient connection state = (state { stateClients = client : stateClients state }, client)
  where
    client = Client { clientId = cid, clientConnection = connection }
    cid = maybe minBound succ $ maximumMay $ map clientId $ stateClients state

removeClient :: Id -> State -> State
removeClient cid state = state { stateClients = filter ((/=) cid . clientId) $ stateClients state }

handleMessage :: Connection -> MVar State -> IO ()
handleMessage connection stateVar = decode <$> receiveData connection >>= \case
    Just message -> modifyMVar_ stateVar $ \state -> do
        world' <- updateWorld message $ stateWorld state
        broadcast world' $ stateClients state
        return state { stateWorld = world' }
    Nothing -> return ()

broadcast :: ToJSON a => a -> [Client] -> IO ()
broadcast message clients = forM_ connections $ flip sendTextData $ encode message
  where
    connections = map clientConnection clients
