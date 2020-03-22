{-# language
    LambdaCase
  , TemplateHaskell
#-}

{-# options_ghc -Wwarn #-}

module Main (main) where

import Control.Monad (void)
import Data.FileEmbed (embedStringFile)
import Data.Foldable (for_)
import Data.List (intercalate)
import System.Directory
import System.Environment (getArgs)
import System.Process (readProcess)

main :: IO ()
main = getArgs >>= \case
  [] -> do
    error "bad"
  (name:_) -> do
    write (project name)
    void $ readProcess "tree" [name] ""

-- | Simple file system tree structure.
data TreeFs
  = Dir FilePath [TreeFs]
    -- ^ Name of directory (relative) and
    --   its containing entries
  | File FilePath String
    -- ^ File name (relative) and file content

write :: TreeFs -> IO ()
write = \case
  File name content -> do
    writeFile name (content <> "\n")
  Dir name children -> do
    createDirectoryIfMissing False name
    withCurrentDirectory name $ for_ children write

language_ :: String -> String
language_ p = "{-# LANGUAGE " <> p <> " #-}"

module_ :: String -> [String] -> String
module_ m is = "module " <> m <> "\n" <> unlines (go is)
  where
    go :: [String] -> [String]
    go [] = ["  (\n  ) where"]
    go (x:xs) = map showExportLine
      (LeftParen x : (map Comma xs ++ [End]))

data ExportLine = LeftParen String | Comma String | End

showExportLine :: ExportLine -> String
showExportLine = \case
  LeftParen i -> "  ( " <> i
  Comma i -> "  , " <> i
  End -> "  ) where"

import_ :: String -> Maybe [String] -> String
import_ m Nothing = "import " <> m
import_ m (Just is) = "import " <> m <> " (" <> intercalate ", " is <> ")"

importQ_ :: String -> Maybe String -> String
importQ_ m Nothing = "import qualified " <> m
importQ_ m (Just q) = "import qualified " <> m <> " as " <> q

project :: String -> TreeFs
project name = Dir name
  [ File (name <> ".cabal") (cabalFile name)
  , Dir "src"
      [ client
      , common
      , server
      ]
  , Dir "app"
      [ File "Main.hs" $ unlines
          [ "module Main (main) where"
          , ""
          , "import qualified Server"
          , ""
          , "main :: IO ()"
          , "main = Server.main"
          ]
      ]
  , defaultNix name
  , shellNix name
  , Dir ".nix"
      [ File "nixpkgs.json" $(embedStringFile "./res/nixpkgs.json")
      , File "pinned-nixpkgs.nix" $(embedStringFile "./res/pinned-nixpkgs.nix")
      , File "trasa.nix" $(embedStringFile "./res/trasa.nix")
      , File "trasa-client.nix" $(embedStringFile "./res/trasa-client.nix")
      , File "trasa-server.nix" $(embedStringFile "./res/trasa-server.nix")
      ]
  , File "Makefile"
      [ "package = " <> name
      , ""
      , "build:"
      , "\tcabal build"
      , ""
      , "clean:"
      , "\tcabal clean"
      , ""
      , "haddock:"
      , "\tcabal haddock"
      , ""
      , "ghci:"
      , "\tcabal repl"
      , ""
      , "ghcid:"
      , "\tghcid -c cabal repl -r \"Server.main\""
      ]
  ]

client :: TreeFs
client = File "Client.hs" $ unlines
  [ language_ "OverloadedStrings"
  , ""
  , module_ "Client" ["helloWorld"]
  , import_ "Lucid" Nothing
  , import_ "Data.IORef" (Just ["IORef", "newIORef", "readIORef"])
  , import_ "Network.HTTP.Client" (Just ["newManager", "defaultManagerSettings"])
  , import_ "Trasa.Core" Nothing
  , import_ "Trasa.Client" (Just ["Scheme(..)", "Authority(..)", "Config(..)", "clientWith"])
  , import_ "System.IO.Unsafe" (Just ["unsafePerformIO"])
  , ""
  , import_ "Common" Nothing
  , ""
  , "scheme :: Scheme"
  , "scheme = Http"
  , ""
  , "authority :: Authority"
  , "authority = Authority scheme \"127.0.0.1\" (Just 8080)"
  , ""
  , "config :: IORef Config"
  , "config = unsafePerformIO $ do"
  , "  mngr <- newManager defaultManagerSettings"
  , "  newIORef (Config authority mempty mngr)"
  , "{-# NOINLINE config #-}"
  , ""
  , "client :: Prepared Route response -> IO (Either TrasaErr response)"
  , "client p = do"
  , "  cfg <- readIORef config"
  , "  clientWith (metaCodecToMetaClient . meta) cfg p"
  , ""
  , "prepare :: ()"
  , "  => Route captures queries request response"
  , "  -> Arguments captures queries request (Prepared Route response)"
  , "prepare = prepareWith meta"
  , ""
  , "helloWorld :: IO (Either TrasaErr (Html ()))"
  , "helloWorld = client $ prepare HelloWorld"
  ]

common :: TreeFs
common = File "Common.hs" $ unlines
  [ language_ "DataKinds"
  , language_ "GADTs"
  , language_ "KindSignatures"
  , language_ "LambdaCase"
  , language_ "OverloadedStrings"
  , language_ "TemplateHaskell"
  , ""
  , module_ "Common"
      [ "Route(..)"
      , "allRoutes"
      , "meta"
      ]
  , import_ "Data.Kind" (Just ["Type"])
  , import_ "Data.String" (Just ["fromString"])
  , import_ "Lucid" Nothing
  , import_ "Trasa.Core" Nothing
  , importQ_ "Data.ByteString.Lazy.Char8" (Just "BC8")
  , importQ_ "Trasa.Method" (Just "Method")
  , ""
  , "data Route :: [Type] -> [Param] -> Bodiedness -> Type -> Type where"
  , "  HelloWorld :: Route '[] '[] 'Bodyless (Html ())"
  , ""
  , "meta :: ()"
  , "  => Route     captures queries request response"
  , "  -> MetaCodec captures queries request response"
  , "meta = \\case"
  , "  HelloWorld -> Meta (match \"hello\" ./ end) qend bodyless (resp (one bodyHtml)) Method.get"
  , ""
  , "bodyHtml :: BodyCodec (Html ())"
  , "bodyHtml = BodyCodec"
  , "  (pure \"text/html\")"
  , "  Lucid.renderBS"
  , "  (Right . fromString . BC8.unpack)"
  , ""
  , "-- Generate all of our routes"
  , "$(generateAllRoutes ''Route)"
  ]

server :: TreeFs
server = File "Server.hs" $ unlines
  [ language_ "DataKinds"
  , language_ "GADTs"
  , language_ "KindSignatures"
  , language_ "LambdaCase"
  , language_ "OverloadedStrings"
  , language_ "ScopedTypeVariables"
  , ""
  , module_ "Server" ["main"]
  , import_ "Data.Functor.Identity" (Just ["Identity"])
  , import_ "Lucid" Nothing
  , import_ "Network.Wai" (Just ["Application"])
  , import_ "Network.Wai.Handler.Warp" (Just ["run"])
  , import_ "Network.Wai.Middleware.RequestLogger" (Just ["logStdoutDev"])
  , import_ "Trasa.Core" Nothing
  , import_ "Trasa.Server" (Just ["TrasaT", "serveWith"])
  , ""
  , import_ "Common" Nothing
  , ""
  , "type App = TrasaT IO"
  , ""
  , "main :: IO ()"
  , "main = run 8080 (logStdoutDev application)"
  , ""
  , "application :: Application"
  , "application = serveWith"
  , "  (metaCodecToMetaServer . meta)"
  , "  routes"
  , "  router"
  , ""
  , "routes :: forall captures queries request response. ()"
  , "  => Route captures queries request response"
  , "  -> Rec Identity captures"
  , "  -> Rec Parameter queries"
  , "  -> RequestBody Identity request"
  , "  -> App response"
  , "routes route captures queries reqBody = case route of"
  , "  HelloWorld -> go helloWorld"
  , "  where"
  , "    go :: Arguments captures queries request (App response) -> App response"
  , "    go f = handler captures queries reqBody f"
  , ""
  , "router :: Router Route"
  , "router = routerWith"
  , "  (mapMeta captureDecoding captureDecoding id id . meta)"
  , "  allRoutes"
  , ""
  , "helloWorld :: App (Html ())"
  , "helloWorld = pure $ h1_ \"Hello, World!\""
  ]

cabalFile :: ()
  => String
     -- ^ library name
  -> String
cabalFile name = unlines
  [ "cabal-version: 2.2"
  , "name:"
  , "  " <> name
  , "version:"
  , "  0.1"
  , "build-type:"
  , "  Simple"
  , ""
  , "library"
  , "  hs-source-dirs:"
  , "    src"
  , "  exposed-modules:"
  , "    Client"
  , "    Common"
  , "    Server"
  , "  build-depends:"
  , "    , aeson"
  , "    , base >= 4.11 && < 4.15"
  , "    , bytestring"
  , "    , http-client"
  , "    , lucid"
  , "    , quantification"
  , "    , text"
  , "    , trasa"
  , "    , trasa-client"
  , "    , trasa-server"
  , "    , wai"
  , "    , wai-extra"
  , "    , warp"
  , "  ghc-options:"
  , "    -Wall -O2"
  , "  default-language:"
  , "    Haskell2010"
  , ""
  , "executable " <> name
  , "  hs-source-dirs:"
  , "    app"
  , "  main-is:"
  , "    Main.hs"
  , "  build-depends:"
  , "    , base"
  , "    , " <> name
  , "  ghc-options:"
  , "    -Wall -O2"
  , "  default-language:"
  , "    Haskell2010"
  ]

defaultNix :: String -> TreeFs
defaultNix name = File "default.nix" $ unlines
  [ "{ system ? builtins.currentSystem"
  , ", compiler ? \"ghc865\""
  , ", ..."
  , "}:"
  , ""
  , "with rec {"
  , "  pkgs = import ./.nix/pinned-nixpkgs.nix {"
  , "    inherit system;"
  , "    config = {"
  , "      allowUnfree = true;"
  , "      packageOverrides = pkgs: rec {"
  , "        haskellPackages = pkgs.haskell.packages.\"${compiler}\".override {"
  , "          overrides = hself: hsuper:"
  , "          with pkgs.haskell.lib; rec {"
  , "            trasa = hself.callPackage ./.nix/trasa.nix {};"
  , "            trasa-client = hself.callPackage ./.nix/trasa-client.nix {};"
  , "            trasa-server = hself.callPackage ./.nix/trasa-server.nix {};"
  , "          };"
  , "        };"
  , "      };"
  , "    };"
  , "  };"
  , ""
  , "  src = pkgs.lib.cleanSource ./.;"
  , "};"
  , ""
  , "rec {"
  , "  " <> name <> " ="
  , "    with pkgs.haskell.lib;"
  , "    with pkgs.haskellPackages;"
  , "    overrideCabal ("
  , "      justStaticExecutables ("
  , "        callCabal2nix \"" <> name <> "\" src {}"
  , "      )"
  , "    ) (old: {"
  , "    });"
  , "}"
  ]

shellNix :: String -> TreeFs
shellNix name = File "shell.nix" $ unlines
  [ "(import ./default.nix {})." <> name <> ".env"
  ]
