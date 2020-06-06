{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Debug.Trace
import Destiny.Types
import Network.WebSockets
import Safe.Foldable

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
    state <- newMVar emptyState
    runServer "127.0.0.1" 3000 $ \pending -> do
        connection <- acceptRequest pending
        client <- modifyMVar state $ return . addClient connection
        finally
            (forever $ handleMessage connection state)
            (modifyMVar_ state $ return . removeClient (clientId client))

addClient :: Connection -> State -> (State, Client)
addClient connection state = (state { stateClients = client : stateClients state }, client)
  where
    client = Client { clientId = cid, clientConnection = connection }
    cid = maybe minBound succ $ maximumMay $ map clientId $ stateClients state

removeClient :: Id -> State -> State
removeClient cid state = state { stateClients = filter ((/=) cid . clientId) $ stateClients state }

handleMessage :: Connection -> MVar State -> IO ()
handleMessage connection state = decode <$> receiveData connection >>= \case
    Just message -> do
        world <- stateWorld <$> readMVar state
        world' <- update world message
        broadcast world'
    Nothing -> return ()
  where
    broadcast message = do
        connections <- map clientConnection <$> stateClients <$> readMVar state
        forM_ connections $ flip sendTextData $ encode message

update :: World -> ClientRequest -> IO World
update world = \case
    AddEntity -> return $ trace "AddEntity" world
    ToggleEntity _ -> return $ trace "ToggleEntity" world
    RemoveEntity _ -> return $ trace "RemoveEntity" world
    AddAspect _ -> return $ trace "AddAspect" world
    EditAspect _ -> return $ trace "EditAspect" world
    RemoveAspect _ -> return $ trace "RemoveAspect" world
    AddDie _ -> return $ trace "AddDie" world
    ToggleDie _ _ _ -> return $ trace "ToggleDie" world
    RemoveDie _ -> return $ trace "RemoveDie" world
    Roll _ -> return $ trace "Roll" world
