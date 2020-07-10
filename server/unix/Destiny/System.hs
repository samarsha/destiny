module Destiny.System (setUserName) where

import System.Posix.User

setUserName :: String -> IO ()
setUserName name = setUserID =<< userID <$> getUserEntryForName name
