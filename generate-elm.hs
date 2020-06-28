{- stack runghc
   --package destiny
   --package directory
   --package elm-bridge
   --package filepath
   --package FindBin
-}

{-# LANGUAGE LambdaCase #-}

import Control.Arrow
import Data.Proxy
import Destiny.Model
import Elm.Module
import Elm.TyRep
import Elm.Versions
import System.Directory
import System.Environment.FindBin
import System.FilePath

main :: IO ()
main = do
    outputDir <- generatedDir <$> getProgPath
    createDirectoryIfMissing True outputDir
    writeFile (outputDir </> "Model.elm") $ makeModule "Destiny.Generated.Model" defs
  where
    generatedDir projectDir = projectDir </> "client" </> "src" </> "Destiny" </> "Generated"

defs :: [DefineElm]
defs =
    [ DefineElm (Proxy :: Proxy Aspect)
    , DefineElm (Proxy :: Proxy ClientRequest)
    , DefineElm (Proxy :: Proxy Entity)
    , DefineElm (Proxy :: Proxy WorldSnapshot)
    ]

makeModule :: String -> [DefineElm] -> String
makeModule name defs = unlines
    [ moduleHeader Elm0p18 name
    , ""
    , "import Dict exposing (Dict)"
    , "import Json.Decode exposing (Decoder)"
    , "import Json.Encode exposing (Value)"
    , "import Json.Helpers exposing (..)"
    , "import Set exposing (Set)"
    , "import Uuid exposing (Uuid)"
    , ""
    , "jsonDecUuid : Decoder Uuid"
    , "jsonDecUuid = Uuid.decoder"
    , ""
    , "jsonEncUuid : Uuid -> Value"
    , "jsonEncUuid = Uuid.encode"
    , ""
    ] ++ makeModuleContentWithAlterations (renameType "UUID" "Uuid") defs

renameType :: String -> String -> ETypeDef -> ETypeDef
renameType before after = \case
    ETypeAlias (alias@EAlias { ea_fields = fields }) -> ETypeAlias $ alias
        { ea_fields = map (second rename) fields }
    typeDef -> typeDef
  where
    rename (ETyCon (ETCon name))
        | name == before = ETyCon $ ETCon after
        | otherwise      = ETyCon $ ETCon name
    rename t = t
