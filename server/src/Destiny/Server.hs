{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Destiny.Server (ServerOptions (..), run) where

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
import Network.Wai.Handler.WebSockets
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.User
import System.Signal
import Text.Printf
import Time.Rational
import Time.Units

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

data ServerOptions = ServerOptions
    { serverPort :: Warp.Port
    , serverStorage :: FilePath
    , serverUser :: Maybe String
    }

data ServerState = ServerState
    { serverWorld :: World
    , serverClients :: [Client]
    }

data Client = Client
    { clientId :: UUID
    , clientConnection :: WS.Connection
    }

run :: ServerOptions -> IO ()
run options@(ServerOptions _ storage _) = do
    savedWorld <- readSavedWorld storage
    stateVar <- newMVar $ newState $ fromMaybe emptyWorld savedWorld
    _ <- forkIO $ saveWorldEvery saveInterval storage stateVar
    let settings = serverSettings options stateVar
    Warp.runSettings settings $ websocketsOr WS.defaultConnectionOptions
        (webSocketsApp stateVar)
        httpApp
  where
    saveInterval = sec 30

serverSettings :: ServerOptions -> MVar ServerState -> Warp.Settings
serverSettings (ServerOptions port storage user) stateVar = Warp.defaultSettings
    & Warp.setPort port
    & Warp.setBeforeMainLoop beforeMainLoop
    & Warp.setInstallShutdownHandler installShutdownHandler
    & Warp.setGracefulShutdownTimeout (Just 5)
  where
    beforeMainLoop = do
        putStrLn $ printf "Listening on port %d." port
        case user of
            Just user' -> do
                putStrLn $ printf "Switching to user %s." user'
                setUserID =<< userID <$> getUserEntryForName user'
            Nothing -> return ()
    installShutdownHandler closeSocket = installHandler sigINT $ const $ do
        putStrLn "Shutting down."
        world <- serverWorld <$> readMVar stateVar
        catchIOError (saveWorld storage world) $ hPutStrLn stderr . show
        closeSocket

newState :: World -> ServerState
newState world = ServerState { serverWorld = world, serverClients = [] }

clientAppDir :: [(FilePath, ByteString)]
clientAppDir = $(embedDir $ "client" </> "app")

worldSavePath :: FilePath -> FilePath
worldSavePath storage = storage </> "world.json"

saveWorld :: FilePath -> World -> IO ()
saveWorld storage world = do
    createDirectoryIfMissing True storage
    LBS.writeFile path $ encodePretty' format world
  where
    path = worldSavePath storage
    format = defConfig { confIndent = Spaces 2 }

saveWorldEvery :: KnownDivRat unit Microsecond => Time unit -> FilePath -> MVar ServerState -> IO ()
saveWorldEvery interval storage stateVar = forever $ do
    threadDelay interval
    world <- modifyMVar stateVar $ \state ->
        let state' = state { serverWorld = commit $ serverWorld state }
        in  return (state', serverWorld state')
    catchIOError (saveWorld storage world) $ hPutStrLn stderr . show

readSavedWorld :: FilePath -> IO (Maybe World)
readSavedWorld storage =
    catchIOError (decodeStrict <$> BS.readFile (worldSavePath storage)) $ \err ->
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

webSocketsApp :: MVar ServerState -> WS.ServerApp
webSocketsApp stateVar pending = do
    connection <- WS.acceptRequest pending
    WS.withPingThread connection (toNum @Second pingInterval) (return ()) $ do
        client <- modifyMVar stateVar $ \state@ServerState { serverWorld = world } -> do
            WS.sendTextData connection $ encode $ worldSnapshot world
            evalRandIO $ addClient connection state
        finally
            (forever $ handleMessage connection stateVar)
            (modifyMVar_ stateVar $ return . removeClient (clientId client))
  where
    pingInterval = sec 30

addClient :: RandomGen g => WS.Connection -> ServerState -> Rand g (ServerState, Client)
addClient connection state@ServerState { serverClients = clients } = do
    newId <- getRandom
    let client = Client { clientId = newId, clientConnection = connection }
    return (state { serverClients = client : clients }, client)

removeClient :: UUID -> ServerState -> ServerState
removeClient uuid state@ServerState { serverClients = clients } = state
    { serverClients = filter (\client -> clientId client /= uuid) clients }

handleMessage :: WS.Connection -> MVar ServerState -> IO ()
handleMessage connection stateVar = decode <$> WS.receiveData connection >>= \case
    Just message -> modifyMVar_ stateVar $ \state -> do
        world' <- evalRandIO $ updateWorld message $ serverWorld state
        broadcast (worldSnapshot world') $ serverClients state
        return state { serverWorld = world' }
    Nothing -> return ()

broadcast :: ToJSON a => a -> [Client] -> IO ()
broadcast message clients = forM_ connections $ flip WS.sendTextData $ encode message
  where
    connections = map clientConnection clients
