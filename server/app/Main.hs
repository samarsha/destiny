{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent hiding (threadDelay)
import Control.Exception
import Control.Monad
import Control.Monad.Random
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.FileEmbed
import Data.Function
import Data.Maybe
import Data.UUID
import Destiny.Model
import Network.HTTP.Types
import Network.Mime
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.Signal
import Text.Printf
import Time.Rational
import Time.Units

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text

data ServerState = ServerState
    { serverWorld :: World
    , serverClients :: [Client]
    }

data Client = Client
    { clientId :: UUID
    , clientConnection :: Connection
    }

main :: IO ()
main = do
    savedWorld <- readSavedWorld
    stateVar <- newMVar $ newState $ fromMaybe emptyWorld savedWorld
    _ <- forkIO $ saveWorldEvery saveInterval stateVar
    let settings = serverSettings stateVar
    putStrLn $ printf "Listening on port %d." $ getPort settings
    runSettings settings $ websocketsOr defaultConnectionOptions (webSocketsApp stateVar) httpApp
  where
    saveInterval = sec 30

serverSettings :: MVar ServerState -> Settings
serverSettings stateVar = defaultSettings
    & setInstallShutdownHandler installShutdownHandler
    & setGracefulShutdownTimeout (Just 5)
  where
    installShutdownHandler closeSocket = installHandler sigINT $ const $ do
        putStrLn "Shutting down."
        saveWorld =<< serverWorld <$> readMVar stateVar
        closeSocket

newState :: World -> ServerState
newState world = ServerState { serverWorld = world, serverClients = [] }

clientAppDir :: [(FilePath, ByteString)]
clientAppDir = $(embedDir $ "client" </> "app")

getWorldSavePath :: IO FilePath
getWorldSavePath = do
    saveFile <- getXdgDirectory XdgData "destiny"
    return $ saveFile </> "world.json"

saveWorld :: World -> IO ()
saveWorld world = do
    path <- getWorldSavePath
    LBS.writeFile path $ encodePretty' format world
  where
    format = defConfig { confIndent = Spaces 2 }

saveWorldEvery :: KnownDivRat unit Microsecond => Time unit -> MVar ServerState -> IO ()
saveWorldEvery interval stateVar = forever $ do
    threadDelay interval
    world <- modifyMVar stateVar $ \state ->
        let state' = state { serverWorld = commit $ serverWorld state }
        in  return (state', serverWorld state')
    catchIOError (saveWorld world) $ hPutStrLn stderr . show

readSavedWorld :: IO (Maybe World)
readSavedWorld = do
    path <- getWorldSavePath
    catchIOError (decodeStrict <$> BS.readFile path) $ \err ->
        if isDoesNotExistError err
        then return Nothing
        else ioError err

httpApp :: Application
httpApp request respond = respond $ case rawPathInfo request of
    "/" -> response "index.html"
    path -> response $ BS.unpack $ BS.tail path
  where
    response path = case lookup path clientAppDir of
        Just content ->
            let contentType = defaultMimeLookup $ Text.pack path
            in responseBuilder status200 [("Content-Type", contentType)] $ byteString content
        Nothing -> responseLBS status404 [("Content-Type", "text/plain")] "404 Not Found"

webSocketsApp :: MVar ServerState -> ServerApp
webSocketsApp stateVar pending = do
    connection <- acceptRequest pending
    withPingThread connection (toNum @Second pingInterval) (return ()) $ do
        client <- modifyMVar stateVar $ \state@ServerState { serverWorld = world } -> do
            sendTextData connection $ encode $ worldSnapshot world
            evalRandIO $ addClient connection state
        finally
            (forever $ handleMessage connection stateVar)
            (modifyMVar_ stateVar $ return . removeClient (clientId client))
  where
    pingInterval = sec 30

addClient :: RandomGen g => Connection -> ServerState -> Rand g (ServerState, Client)
addClient connection state@ServerState { serverClients = clients } = do
    newId <- getRandom
    let client = Client { clientId = newId, clientConnection = connection }
    return (state { serverClients = client : clients }, client)

removeClient :: UUID -> ServerState -> ServerState
removeClient uuid state@ServerState { serverClients = clients } = state
    { serverClients = filter (\client -> clientId client /= uuid) clients }

handleMessage :: Connection -> MVar ServerState -> IO ()
handleMessage connection stateVar = decode <$> receiveData connection >>= \case
    Just message -> modifyMVar_ stateVar $ \state -> do
        world' <- evalRandIO $ updateWorld message $ serverWorld state
        broadcast (worldSnapshot world') $ serverClients state
        return state { serverWorld = world' }
    Nothing -> return ()

broadcast :: ToJSON a => a -> [Client] -> IO ()
broadcast message clients = forM_ connections $ flip sendTextData $ encode message
  where
    connections = map clientConnection clients
