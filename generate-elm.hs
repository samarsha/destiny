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
    , DefineElm (Proxy :: Proxy AspectId)
    , DefineElm (Proxy :: Proxy ClientRequest)
    , DefineElm (Proxy :: Proxy Entity)
    , DefineElm (Proxy :: Proxy EntityId)
    , DefineElm (Proxy :: Proxy Event)
    , DefineElm (Proxy :: Proxy RollId)
    , DefineElm (Proxy :: Proxy Scene)
    , DefineElm (Proxy :: Proxy Stat)
    , DefineElm (Proxy :: Proxy StatGroup)
    , DefineElm (Proxy :: Proxy StatGroupId)
    , DefineElm (Proxy :: Proxy StatId)
    , DefineElm (Proxy :: Proxy WorldSnapshot)
    ]

makeModule :: String -> [DefineElm] -> String
makeModule name defs = unlines
    [ moduleHeader Elm0p18 name
    , ""
    , "import Dict exposing (Dict)"
    , "import Dict.Any exposing (AnyDict)"
    , "import Json.Decode exposing (Decoder)"
    , "import Json.Encode exposing (Value)"
    , "import Json.Helpers exposing (..)"
    , "import Set exposing (Set)"
    , "import Uuid exposing (Uuid)"
    , ""
    , "type alias UuidDict k v = AnyDict String k v"
    , ""
    , "jsonDecUuid : Decoder Uuid"
    , "jsonDecUuid = Uuid.decoder"
    , ""
    , "jsonEncUuid : Uuid -> Value"
    , "jsonEncUuid = Uuid.encode"
    , ""
    , "jsonDecUuidDict : a -> Decoder v -> Decoder (UuidDict Uuid v)"
    , "jsonDecUuidDict _ valueDecoder ="
    , "  let"
    , "    insert key value dict = case Uuid.fromString key of"
    , "      Just k -> dict |> Dict.Any.insert k value |> Json.Decode.succeed"
    , "      Nothing -> \"Key '\" ++ key ++ \"' cannot be converted to a UUID.\" |> Json.Decode.fail"
    , "    create key value acc = acc |> Json.Decode.andThen (insert key value)"
    , "  in"
    , "    Json.Decode.dict valueDecoder"
    , "    |> Json.Decode.andThen (Dict.foldr create (Dict.Any.empty Uuid.toString |> Json.Decode.succeed))"
    , ""
    , "jsonEncUuidDict : (k -> Value) -> (v -> Value) -> UuidDict k v -> Value"
    , "jsonEncUuidDict encodeKey encodeValue ="
    , "  Dict.Any.encode (encodeKey >> Json.Encode.encode 0) encodeValue"
    , ""
    ] ++
    makeModuleContentWithAlterations
        ( renameType "UUID" "Uuid"
        . renameType "Map" "UuidDict"
        )
        defs

renameType :: String -> String -> ETypeDef -> ETypeDef
renameType before after = \case
    ETypeAlias (alias@EAlias { ea_fields = fields }) -> ETypeAlias $ alias
        { ea_fields = map (second rename) fields }
    ETypePrimAlias (alias@EPrimAlias { epa_type = eType }) -> ETypePrimAlias $ alias
        { epa_type = rename eType }
    ETypeSum (eSum@ESum { es_constructors = constructors }) -> ETypeSum $ eSum
        { es_constructors = map updateSumConstructor constructors }
  where
    rename (ETyCon (ETCon name))
        | name == before = ETyCon $ ETCon after
        | otherwise      = ETyCon $ ETCon name
    rename (ETyApp lhs rhs) = ETyApp (rename lhs) (rename rhs)
    rename eType = eType

    updateSumConstructor (constructor@STC { _stcFields = fields }) = constructor
        { _stcFields = updateSumFields fields }

    updateSumFields (Anonymous types) = Anonymous $ map rename types
    updateSumFields (Named fields) = Named $ map (second rename) fields
