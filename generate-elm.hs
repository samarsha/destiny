{- stack runghc
   --package destiny
   --package directory
   --package elm-bridge
   --package filepath
   --package FindBin
-}

import Data.Proxy
import Destiny.Model
import Elm.Module
import System.Directory
import System.Environment.FindBin
import System.FilePath

main :: IO ()
main = do
    outputDir <- generatedDir <$> getProgPath
    createDirectoryIfMissing True outputDir
    writeFile (outputDir </> "Model.elm") $ makeElmModule "Destiny.Generated.Model" definitions
  where
    generatedDir projectDir = projectDir </> "client" </> "src" </> "Destiny" </> "Generated"

definitions :: [DefineElm]
definitions =
    [ DefineElm (Proxy :: Proxy Aspect)
    , DefineElm (Proxy :: Proxy ClientRequest)
    , DefineElm (Proxy :: Proxy Entity)
    , DefineElm (Proxy :: Proxy Id)
    , DefineElm (Proxy :: Proxy World)
    ]
