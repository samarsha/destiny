{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Text (Text)
import Destiny.Types
import Network.WebSockets
import Safe.Foldable

data Client = Client
    { clientId :: Id
    , clientConnection :: Connection
    }

main :: IO ()
main = do
    clients <- newMVar []
    runServer "127.0.0.1" 3000 $ \pending -> do
        connection <- acceptRequest pending
        client <- modifyMVar clients $ return . addClient connection
        finally
            (forever $ handleMessage connection clients)
            (modifyMVar_ clients $ return . removeClient (clientId client))

handleMessage :: Connection -> MVar [Client] -> IO ()
handleMessage connection clients = do
    message <- receiveData connection :: IO Text
    mapM_ (flip sendTextData message) =<< map clientConnection <$> readMVar clients

addClient :: Connection -> [Client] -> ([Client], Client)
addClient connection clients = (client : clients, client)
  where
    client = Client { clientId = cid, clientConnection = connection }
    cid = maybe minBound succ $ maximumMay $ map clientId clients

removeClient :: Id -> [Client] -> [Client]
removeClient cid clients = filter ((/=) cid . clientId) clients
