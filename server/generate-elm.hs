{- stack runghc
   --package destiny-server
   --package directory
   --package elm-bridge
   --package filepath
   --package FindBin
-}

import Data.Proxy
import Destiny.Types
import Elm.Module
import System.Directory
import System.Environment.FindBin
import System.FilePath

main :: IO ()
main = do
    outputDir <- generatedDir <$> takeDirectory <$> getProgPath
    createDirectoryIfMissing True outputDir
    writeFile (outputDir </> "Types.elm") $ makeElmModule "Destiny.Generated.Types"
        [ DefineElm (Proxy :: Proxy World)
        , DefineElm (Proxy :: Proxy Entity)
        , DefineElm (Proxy :: Proxy Aspect)
        , DefineElm (Proxy :: Proxy Request)
        ]
  where
    generatedDir projectDir = projectDir </> "client" </> "src" </> "Destiny" </> "Generated"
