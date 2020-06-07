{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Destiny.Model
import Network.WebSockets
import Safe.Foldable
import Text.Printf

data State = State
    { stateWorld :: World
    , stateClients :: [Client]
    }

data Client = Client
    { clientId :: Id
    , clientConnection :: Connection
    }

emptyState :: State
emptyState = State
    { stateWorld = World
        { worldEntities = []
        , worldLastRoll = 0
        }
    , stateClients = []
    }

main :: IO ()
main = do
    stateVar <- newMVar emptyState
    putStrLn $ printf "Listening on %s:%d." hostName port
    runServer hostName port $ \pending -> do
        connection <- acceptRequest pending
        client <- modifyMVar stateVar $ return . addClient connection
        finally
            (forever $ handleMessage connection stateVar)
            (modifyMVar_ stateVar $ return . removeClient (clientId client))
  where
    hostName = "localhost"
    port = 3000

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
