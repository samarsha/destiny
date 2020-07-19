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
import Elm.Module
import Elm.TyRep
import Elm.Versions
import System.Directory
import System.Environment.FindBin
import System.FilePath

import qualified Destiny.Message as Message
import qualified Destiny.Scene as Scene
import qualified Destiny.Server as Server
import qualified Destiny.World as World

data Module = Module
    { name :: String
    , dependencies :: [String]
    , definitions :: [DefineElm]
    }

main :: IO ()
main = do
    outputDir <- generatedDir <$> getProgPath
    createDirectoryIfMissing True outputDir
    mapM_ (writeModule outputDir)
        [ Module "Message" ["Scene"] messageDefs
        , Module "Scene" [] sceneDefs
        , Module "Server" ["Scene", "World"] serverDefs
        , Module "World" ["Message", "Scene"] worldDefs
        ]
  where
    generatedDir projectDir = projectDir </> "client" </> "src" </> "Destiny" </> "Generated"

writeModule :: FilePath -> Module -> IO ()
writeModule outputDir mod = writeFile fileName $ makeModule mod
    where
    fileName = outputDir </> name mod -<.> "elm"

fullName :: String -> String
fullName name = "Destiny.Generated." ++ name

messageDefs :: [DefineElm]
messageDefs =
    [ DefineElm (Proxy :: Proxy Message.Invoke)
    , DefineElm (Proxy :: Proxy Message.Message)
    , DefineElm (Proxy :: Proxy Message.MessageId)
    , DefineElm (Proxy :: Proxy Message.MessageList)
    , DefineElm (Proxy :: Proxy Message.Roll)
    ]

sceneDefs :: [DefineElm]
sceneDefs =
    [ DefineElm (Proxy :: Proxy Scene.Aspect)
    , DefineElm (Proxy :: Proxy Scene.AspectId)
    , DefineElm (Proxy :: Proxy Scene.Die)
    , DefineElm (Proxy :: Proxy Scene.Entity)
    , DefineElm (Proxy :: Proxy Scene.EntityId)
    , DefineElm (Proxy :: Proxy Scene.Role)
    , DefineElm (Proxy :: Proxy Scene.Scene)
    , DefineElm (Proxy :: Proxy Scene.Stat)
    , DefineElm (Proxy :: Proxy Scene.StatGroup)
    , DefineElm (Proxy :: Proxy Scene.StatGroupId)
    , DefineElm (Proxy :: Proxy Scene.StatId)
    ]

serverDefs :: [DefineElm]
serverDefs =
    [ DefineElm (Proxy :: Proxy Server.ServerCommand)
    ]

worldDefs :: [DefineElm]
worldDefs =
    [ DefineElm (Proxy :: Proxy World.Command)
    , DefineElm (Proxy :: Proxy World.Snapshot)
    ]

makeModule :: Module -> String
makeModule mod = standardHeader ++ extraImports ++ uuidDict ++ content
  where
    standardHeader = unlines
        [ moduleHeader Elm0p18 $ fullName $ name mod
        , ""
        , "import Destiny.AnyBag as AnyBag exposing (AnyBag)"
        , "import Destiny.Utils exposing (decodeAnyDict)"
        , "import Dict exposing (Dict)"
        , "import Dict.Any exposing (AnyDict)"
        , "import Json.Decode exposing (Decoder)"
        , "import Json.Encode exposing (Value)"
        , "import Json.Helpers exposing (..)"
        , "import Set exposing (Set)"
        , "import Uuid exposing (Uuid)"
        , ""
        ]
    extraImports = unlines $
        map (\name -> "import " ++ fullName name ++ " exposing (..)") $ dependencies mod
    uuidDict = unlines $
        [ ""
        , "type alias UuidDict k v = AnyDict String k v"
        , ""
        , "type alias DiceBag a = AnyBag String a"
        , ""
        , "jsonDecUuid : Decoder Uuid"
        , "jsonDecUuid = Uuid.decoder"
        , ""
        , "jsonEncUuid : Uuid -> Value"
        , "jsonEncUuid = Uuid.encode"
        , ""
        , "jsonDecUuidDict : a -> Decoder v -> Decoder (UuidDict Uuid v)"
        , "jsonDecUuidDict _ valueDecoder ="
        , "  decodeAnyDict (\\k _ -> Uuid.fromString k) Uuid.toString valueDecoder"
        , ""
        , "jsonEncUuidDict : (k -> Value) -> (v -> Value) -> UuidDict k v -> Value"
        , "jsonEncUuidDict encodeKey encodeValue ="
        , "  Dict.Any.encode (encodeKey >> Json.Encode.encode 0) encodeValue"
        , ""
        , "jsonDecDiceBag : Decoder a -> Decoder (DiceBag Die)"
        , "jsonDecDiceBag _ ="
        , "  let quote s = \"\\\"\" ++ s ++ \"\\\"\""
        , "  in"
        , "    AnyBag.decode"
        , "      (quote >> Json.Decode.decodeString jsonDecDie >> Result.toMaybe)"
        , "      (jsonEncDie >> Json.Encode.encode 0)"
        , ""
        , "jsonEncDiceBag : (a -> Value) -> DiceBag a -> Value"
        , "jsonEncDiceBag encode = AnyBag.encode (encode >> Json.Encode.encode 0)"
        , ""
        ]
    content = makeModuleContentWithAlterations
        ( renameType "UUID" "Uuid"
        . renameType "Map" "UuidDict"
        . renameType "MultiSet" "DiceBag"
        )
        (definitions mod)

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
