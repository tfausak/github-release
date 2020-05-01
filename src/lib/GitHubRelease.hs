{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module GitHubRelease
  ( Command(..)
  , main
  , runCommand
  , upload
  , getUploadUrl
  , getTag
  , authorizationHeader
  , userAgentHeader
  , userAgent
  , versionString
  , uploadFile
  , uploadBody
  ) where

import           Options.Generic            (type (<?>))

import qualified Burrito
import           Data.Aeson                 (object, (.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text                  as Text
import qualified Data.Version               as Version
import qualified GHC.Generics               as Generics
import qualified Network.HTTP.Client        as Client
import qualified Network.HTTP.Client.TLS    as TLS
import qualified Network.HTTP.Types         as HTTP
import qualified Network.Mime               as MIME
import qualified Options.Generic            as Options
import qualified Paths_github_release       as This
import qualified System.Environment         as Environment
import qualified System.IO                  as IO
import qualified Text.Printf                as Printf

data Command
  = Upload { file :: FilePath <?> "The path to the local file to upload."
          ,  name :: String <?> "The name to give the file on the release."
          ,  owner :: Maybe String <?> "The GitHub owner, either a user or organization."
          ,  repo :: String <?> "The GitHub repository name."
          ,  tag :: String <?> "The tag name."
          ,  token :: Maybe String <?> "Your OAuth2 token."}
  | Release { title :: String <?> "The name of the release"
          ,  owner :: Maybe String <?> "The GitHub owner, either a user or organization."
          ,  repo :: String <?> "The GitHub repository name."
          ,  tag :: String <?> "The tag name."
          ,  description :: Maybe String <?> "Release description."
          ,  token :: Maybe String <?> "Your OAuth2 token."
          ,  preRelease :: Maybe Bool <?> "Indicates if this is a pre-release."
          ,  draft :: Maybe Bool <?> "Indicates if this is a draft."
          }
  | Delete
    { name :: String <?> "The name to give the file on the release."
    , owner :: Maybe String <?> "The GitHub owner, either a user or organization."
    , repo :: String <?> "The GitHub repository name."
    , tag :: String <?> "The tag name."
    , token :: Maybe String <?> "Your OAuth2 token."
    }
  | Version
  deriving (Generics.Generic, Show)

instance Options.ParseRecord Command

main :: IO ()
main = do
  command <- Options.getRecord (Text.pack "Upload a file to a GitHub release.")
  runCommand command

runCommand :: Command -> IO ()
runCommand command =
  case command of
    Upload aFile aName anOwner aRepo aTag helpfulToken -> do
      aToken <- maybe (Environment.getEnv "GITHUB_TOKEN") pure
        $ Options.unHelpful helpfulToken
      upload
        aToken
        (Options.unHelpful anOwner)
        (Options.unHelpful aRepo)
        (Options.unHelpful aTag)
        (Options.unHelpful aFile)
        (Options.unHelpful aName)
    Release aTitle anOwner aRepo aTag aDescription helpfulToken aPreRelease aDraft -> do
      aToken <- maybe (Environment.getEnv "GITHUB_TOKEN") pure
        $ Options.unHelpful helpfulToken
      release
        aToken
        (Options.unHelpful anOwner)
        (Options.unHelpful aRepo)
        (Options.unHelpful aTag)
        (Options.unHelpful aTitle)
        (Options.unHelpful aDescription)
        (Options.unHelpful aPreRelease)
        (Options.unHelpful aDraft)
    Delete aName anOwner aRepo aTag helpfulToken -> do
      aToken <- maybe (Environment.getEnv "GITHUB_TOKEN") pure
        $ Options.unHelpful helpfulToken
      delete
        (Options.unHelpful aName)
        (Options.unHelpful anOwner)
        (Options.unHelpful aRepo)
        (Options.unHelpful aTag)
        aToken
    Version -> putStrLn versionString

upload :: String -> Maybe String -> String -> String -> FilePath -> String -> IO ()
upload aToken anOwner aRepo aTag aFile aName = do
  manager <- Client.newManager TLS.tlsManagerSettings
  uploadUrl <- getUploadUrl manager aToken anOwner aRepo aTag
  response <- uploadFile manager uploadUrl aToken aFile aName
  case HTTP.statusCode (Client.responseStatus response) of
    201 -> pure ()
    _   -> fail "Failed to upload file to release!"

release :: String -> Maybe String -> String -> String -> String -> Maybe String -> Maybe Bool -> Maybe Bool -> IO ()
release aToken anOwner aRepo aTag aTitle aDescription aPreRelease aDraft = do
  manager <- Client.newManager TLS.tlsManagerSettings
  (owner', repo') <- getOwnerRepo anOwner aRepo
  let format = "https://api.github.com/repos/%s/%s/releases"
  let
    url :: String
    url = Printf.printf format owner' repo'
  response <- mkRelease manager url aToken aTag aTitle aDescription aPreRelease aDraft
  let body = Aeson.eitherDecode $ Client.responseBody response :: Either String Aeson.Object
  case HTTP.statusCode (Client.responseStatus response) of
    201 -> pure ()
    422 -> IO.hPutStrLn IO.stderr "Release aready exists. Ignoring."
    _   -> fail $ "Failed to create release! Reason: " <> (show body)

delete :: String -> Maybe String -> String -> String -> String -> IO ()
delete aName rawOwner rawRepo aTag aToken = do
  manager <- Client.newManager TLS.tlsManagerSettings
  (anOwner, aRepo) <- getOwnerRepo rawOwner rawRepo
  ghRelease <- do
    result <- getTag manager aToken anOwner aRepo aTag
    case result of
      Left problem -> fail $ "Failed to get tag JSON: " ++ show problem
      Right json -> pure json
  case filter ((== aName) . ghAssetName) $ ghReleaseAssets ghRelease of
    [] -> fail $ "Failed to find asset on release."
    ghAsset : _ -> do
      request <- Client.parseRequest $ ghAssetUrl ghAsset
      response <- Client.httpLbs request
        { Client.method = HTTP.methodDelete
        , Client.requestHeaders = [authorizationHeader aToken, userAgentHeader]
        } manager
      case HTTP.statusCode $ Client.responseStatus response of
        204 -> pure ()
        _ -> fail $ "Failed to delete asset from release! " <> show response

newtype GHRelease = GHRelease
  { ghReleaseAssets :: [GHAsset]
  } deriving (Eq, Show)

instance Aeson.FromJSON GHRelease where
  parseJSON = Aeson.withObject "GHRelease" $ \ obj -> GHRelease
    <$> obj Aeson..: Text.pack "assets"

data GHAsset = GHAsset
  { ghAssetName :: String
  , ghAssetUrl :: String
  } deriving (Eq, Show)

instance Aeson.FromJSON GHAsset where
  parseJSON = Aeson.withObject "GHAsset" $ \ obj -> GHAsset
    <$> obj Aeson..: Text.pack "name"
    <*> obj Aeson..: Text.pack "url"

getUploadUrl
  :: Client.Manager
  -> String
  -> Maybe String
  -> String
  -> String
  -> IO Burrito.Template
getUploadUrl manager aToken rawOwner rawRepo aTag = do
  json <- do
    (anOwner, aRepo) <- getOwnerRepo rawOwner rawRepo
    result <- getTag manager aToken anOwner aRepo aTag
    case result of
      Left problem -> fail ("Failed to get tag JSON: " ++ show problem)
      Right json   -> pure json
  text <- case HashMap.lookup (Text.pack "upload_url") json of
    Just (Aeson.String text) -> pure text
    _ -> fail ("Failed to get upload URL: " ++ show json)
  let uploadUrl = Text.unpack text
  template <- case Burrito.parse uploadUrl of
    Nothing        -> fail ("Failed to parse URL template: " ++ show uploadUrl)
    Just template  -> pure template
  pure template

getOwnerRepo :: Maybe String -> String -> IO ((String, String))
getOwnerRepo rawOwner rawRepo = do
  (anOwner, aRepo) <- case break (== '/') rawRepo of
    (aRepo, "") -> case rawOwner of
      Nothing      -> fail "Missing required option --owner."
      Just anOwner -> pure (anOwner, aRepo)
    (anOwner, aRepo) -> do
      case rawOwner of
        Nothing -> pure ()
        Just _  -> IO.hPutStrLn IO.stderr "Ignoring --owner option."
      pure (anOwner, drop 1 aRepo)
  return (anOwner, aRepo)

getTag
  :: Aeson.FromJSON a
  => Client.Manager
  -> String
  -> String
  -> String
  -> String
  -> IO (Either String a)
getTag manager aToken anOwner aRepo aTag = do
  let format = "https://api.github.com/repos/%s/%s/releases/tags/%s"
  let
    url :: String
    url = Printf.printf format anOwner aRepo aTag
  initialRequest <- Client.parseRequest url
  let request =
        initialRequest
        {Client.requestHeaders = [authorizationHeader aToken, userAgentHeader]}
  response <- Client.httpLbs request manager
  let body = Client.responseBody response
  return (Aeson.eitherDecode body)

authorizationHeader :: String -> HTTP.Header
authorizationHeader aToken =
  (HTTP.hAuthorization, BS8.pack (Printf.printf "token %s" aToken))

userAgentHeader :: HTTP.Header
userAgentHeader = (HTTP.hUserAgent, BS8.pack userAgent)

userAgent :: String
userAgent = Printf.printf "%s/%s-%s" "tfausak" "github-release" versionString

versionString :: String
versionString = Version.showVersion This.version

uploadFile
  :: Client.Manager
  -> Burrito.Template
  -> String
  -> FilePath
  -> String
  -> IO (Client.Response BSL.ByteString)
uploadFile manager template aToken aFile aName = do
  contents <- BSL.readFile aFile
  let body = Client.RequestBodyLBS contents
  uploadBody manager template aToken body aName

uploadBody
  :: Client.Manager
  -> Burrito.Template
  -> String
  -> Client.RequestBody
  -> String
  -> IO (Client.Response BSL.ByteString)
uploadBody manager template aToken body aName = do
  let
    url :: String
    url = Burrito.expand
      [("name", Burrito.stringValue aName)]
      template
  initialRequest <- Client.parseRequest url
  let request =
        initialRequest
        { Client.method = BS8.pack "POST"
        , Client.requestBody = body
        , Client.requestHeaders =
            [ authorizationHeader aToken
            , (HTTP.hContentType, MIME.defaultMimeLookup (Text.pack aName))
            , userAgentHeader
            ]
        }
  Client.httpLbs request manager

mkRelease
  :: Client.Manager
  -> String
  -> String
  -> String
  -> String
  -> Maybe String
  -> Maybe Bool
  -> Maybe Bool
  -> IO (Client.Response BSL.ByteString)
mkRelease manager url aToken aTag aTitle aDescription aPreRelease aDraft = do
  initialRequest <- Client.parseRequest url
  let requestObject = object
            [ Text.pack "tag_name" .= aTag
            , Text.pack "name"  .= aTitle
            , Text.pack "body" .= maybe "" id aDescription
            , Text.pack "prerelease" .= maybe False id aPreRelease
            , Text.pack "draft" .= maybe False id aDraft
            ]
  let request =
        initialRequest
        { Client.method = BS8.pack "POST"
        , Client.requestBody = Client.RequestBodyLBS $ Aeson.encode requestObject
        , Client.requestHeaders =
            [ authorizationHeader aToken
            , userAgentHeader
            ]
        }
  Client.httpLbs request manager
