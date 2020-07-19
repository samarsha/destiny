{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Destiny.Server (ServerCommand, ServerOptions (..), run) where

import Control.Concurrent hiding (threadDelay)
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Random
import Data.Aeson hiding (defaultOptions)
import Data.Aeson.Encode.Pretty
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.FileEmbed
import Data.Generics.Labels ()
import Data.Map.Lazy (Map)
import Data.Maybe
import Data.UUID
import Destiny.Scene
import Destiny.System
import Destiny.World (World)
import Elm.Derive
import GHC.Generics
import Network.HTTP.Types
import Network.Mime
import Network.Wai
import Network.Wai.Handler.WebSockets
import System.Directory
import System.FilePath
import System.Info
import System.IO
import System.IO.Error
import System.Signal
import Text.Printf
import Time.Rational
import Time.Units

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Lazy as Map
import qualified Data.Text as Text
import qualified Destiny.World as World
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

data ServerOptions = ServerOptions
    { port :: Warp.Port
    , storage :: FilePath
    , user :: Maybe String
    }

newtype ClientId = ClientId UUID deriving (Eq, Ord, Random)

data Client = Client
    { id :: ClientId
    , connection :: WS.Connection
    , role :: Role
    }
    deriving Generic

data ServerCommand
    = WorldCommand World.Command
    | SetRole Role
deriveBoth defaultOptions ''ServerCommand

data ServerState = ServerState
    { world :: World
    , clients :: Map ClientId Client
    }
    deriving Generic

run :: ServerOptions -> IO ()
run options = do
    savedWorld <- readSavedWorld $ storage options
    stateVar <- newMVar $ newState $ fromMaybe World.empty savedWorld
    _ <- forkIO $ saveWorldEvery saveInterval (storage options) stateVar
    let settings = serverSettings options stateVar
    Warp.runSettings settings $ websocketsOr WS.defaultConnectionOptions
        (webSocketsApp stateVar)
        httpApp
  where
    saveInterval = sec 30

serverSettings :: ServerOptions -> MVar ServerState -> Warp.Settings
serverSettings options stateVar = Warp.defaultSettings
    & Warp.setPort (port options)
    & Warp.setBeforeMainLoop beforeMainLoop
    & Warp.setInstallShutdownHandler installShutdownHandler
    & Warp.setGracefulShutdownTimeout (Just 5)
  where
    beforeMainLoop = do
        putStrLn $ printf "Listening on port %d." $ port options
        case user options of
            Just name' | os == "mingw32" ->
                putStrLn $ printf "Switching to user %s is not supported on Windows." name'
            Just name' -> do
                putStrLn $ printf "Switching to user %s." name'
                setUserName name'
            Nothing -> return ()
    installShutdownHandler closeSocket = installHandler sigINT $ const $ do
        putStrLn "Shutting down."
        state <- readMVar stateVar
        catchIOError (saveWorld (storage options) $ world state) $ hPutStrLn stderr . show
        closeSocket

newState :: World -> ServerState
newState world' = ServerState { world = world', clients = Map.empty }

clientAppDir :: [(FilePath, ByteString)]
clientAppDir = $(embedDir $ "client" </> "app")

worldSavePath :: FilePath -> FilePath
worldSavePath storage' = storage' </> "world.json"

saveWorld :: FilePath -> World -> IO ()
saveWorld storage' world' = do
    createDirectoryIfMissing True storage'
    LBS.writeFile path $ encodePretty' format world'
  where
    path = worldSavePath storage'
    format = defConfig { confIndent = Spaces 2 }

saveWorldEvery :: KnownDivRat unit Microsecond => Time unit -> FilePath -> MVar ServerState -> IO ()
saveWorldEvery interval storage' stateVar = forever $ do
    threadDelay interval
    world' <- modifyMVar stateVar $ \state ->
        let state' = state & over #world World.commit
        in return (state', world state')
    catchIOError (saveWorld storage' world') $ hPutStrLn stderr . show

readSavedWorld :: FilePath -> IO (Maybe World)
readSavedWorld storage' =
    catchIOError (decodeStrict <$> BS.readFile (worldSavePath storage')) $ \err ->
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
    connection' <- WS.acceptRequest pending
    WS.withPingThread connection' (toNum @Second pingInterval) (return ()) $ do
        client <- modifyMVar stateVar $ \state -> do
            WS.sendTextData connection' $ encode $ World.snapshot $ world state
            evalRandIO $ addClient connection' state
        finally
            (forever $ receiveMessage (client ^. #connection) (client ^. #id) stateVar)
            (modifyMVar_ stateVar $ return . removeClient (client ^. #id))
  where
    pingInterval = sec 30

addClient :: RandomGen g => WS.Connection -> ServerState -> Rand g (ServerState, Client)
addClient connection' state = do
    clientId <- getRandom
    let client = Client { id = clientId, connection = connection', role = Player }
    let state' = state & over #clients (Map.insert clientId client)
    return (state', client)

removeClient :: ClientId -> ServerState -> ServerState
removeClient clientId = over #clients $ Map.delete clientId

receiveMessage :: WS.Connection -> ClientId -> MVar ServerState -> IO ()
receiveMessage connection' clientId stateVar =
    decode <$> WS.receiveData connection' >>= \case
        Just command -> modifyMVar_ stateVar $ \state ->
            case Map.lookup clientId $ clients state of
                Just client -> handleMessage state client command
                Nothing -> return state
        Nothing -> return ()

handleMessage :: ServerState -> Client -> ServerCommand -> IO ServerState
handleMessage state client = \case
    WorldCommand command -> do
        (world', reply) <- evalRandIO $ World.update (role client) command $ world state
        let clients' = Map.elems $ case reply of
                World.All -> clients state
                World.Others -> Map.delete (client ^. #id) $ clients state
        broadcast (World.snapshot world') clients'
        return state { world = world' }
    SetRole role' ->
        return $ state & over #clients (Map.adjust (#role .~ role') $ client ^. #id)

broadcast :: ToJSON a => a -> [Client] -> IO ()
broadcast message clients' = forM_ connections $ flip WS.sendTextData $ encode message
  where
    connections = map connection clients'
