module Destiny.System (setUserName) where

setUserName :: String -> IO ()
setUserName _ = error "setUserName is not supported on Windows."
