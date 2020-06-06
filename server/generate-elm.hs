{- stack runghc
   --package destiny-server
   --package directory
   --package elm-bridge
   --package filepath
   --package FindBin
-}

{-# LANGUAGE LambdaCase #-}

import Control.Arrow
import Data.Char
import Data.List
import Data.Proxy
import Destiny.Types
import Elm.Module
import Elm.TyRep
import Elm.Versions
import System.Directory
import System.Environment.FindBin
import System.FilePath

main :: IO ()
main = do
    outputDir <- generatedDir <$> takeDirectory <$> getProgPath
    createDirectoryIfMissing True outputDir
    writeFile (outputDir </> "Types.elm") $
        moduleHeaders "Destiny.Generated.Types" ++
        makeModuleContentWithAlterations stripFieldPrefixes definitions
  where
    generatedDir projectDir = projectDir </> "client" </> "src" </> "Destiny" </> "Generated"

definitions :: [DefineElm]
definitions =
    [ DefineElm (Proxy :: Proxy World)
    , DefineElm (Proxy :: Proxy Entity)
    , DefineElm (Proxy :: Proxy Aspect)
    , DefineElm (Proxy :: Proxy Request)
    ]

stripFieldPrefixes :: ETypeDef -> ETypeDef
stripFieldPrefixes = \case
    ETypeAlias alias ->
        let prefix = lowerFirst $ et_name $ ea_name alias
            strip fieldName = maybe fieldName lowerFirst $ stripPrefix prefix fieldName
        in ETypeAlias $ alias { ea_fields = map (first strip) (ea_fields alias) }
    typeDef -> typeDef
  where
    lowerFirst (x : xs) = toLower x : xs

moduleHeaders :: String -> String
moduleHeaders name = unlines
    [ moduleHeader Elm0p18 name
    , ""
    , "import Json.Decode"
    , "import Json.Encode exposing (Value)"
    , "import Json.Helpers exposing (..)"
    , "import Dict exposing (Dict)"
    , "import Set exposing (Set)"
    , ""
    ]
