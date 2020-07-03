module Main (main) where

import Data.Functor
import Data.Maybe
import Destiny.Server
import Options.Applicative
import System.Directory

import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
    defaultStorage <- getXdgDirectory XdgData "destiny"
    run =<< execParser (options defaultStorage)

options :: FilePath -> ParserInfo ServerOptions
options defaultStorage = info (serverOptions defaultStorage <**> helper) fullDesc

serverOptions :: FilePath -> Parser ServerOptions
serverOptions defaultStorage = ServerOptions
    <$> (optional portOption <&> fromMaybe 3000)
    <*> (optional storageOption <&> fromMaybe defaultStorage)

portOption :: Parser Warp.Port
portOption = option auto
    ( long "port"
   <> short 'p'
   <> metavar "NUM"
   <> help "Port number to listen on"
    )

storageOption :: Parser FilePath
storageOption = strOption
    ( long "storage"
   <> metavar "PATH"
   <> help "Directory to use for persistent storage"
    )
