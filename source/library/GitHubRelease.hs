{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module GitHubRelease where

import Options.Generic (type (<?>))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified GHC.Generics as Generics
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTP
import qualified Network.Mime as MIME
import qualified Network.URI.Template as Template
import qualified Network.URI.Template.Types as Template
import qualified Options.Generic as Options
import qualified Paths_github_release as This
import qualified System.IO as IO
import qualified Text.Printf as Printf

data Command
  = Upload { file :: FilePath <?> "The path to the local file to upload."
          ,  name :: String <?> "The name to give the file on the release."
          ,  owner :: String <?> "The GitHub owner, either a user or organization."
          ,  repo :: String <?> "The GitHub repository name."
          ,  tag :: String <?> "The tag name."
          ,  token :: String <?> "Your OAuth2 token."}
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
    Upload aFile aName anOwner aRepo aTag aToken ->
      upload
        (Options.unHelpful aToken)
        (Options.unHelpful anOwner)
        (Options.unHelpful aRepo)
        (Options.unHelpful aTag)
        (Options.unHelpful aFile)
        (Options.unHelpful aName)
    Version -> putStrLn versionString

upload :: String -> String -> String -> String -> FilePath -> String -> IO ()
upload aToken anOwner aRepo aTag aFile aName = do
  manager <- Client.newManager TLS.tlsManagerSettings
  uploadUrl <- getUploadUrl manager aToken anOwner aRepo aTag
  response <- uploadFile manager uploadUrl aToken aFile aName
  case HTTP.statusCode (Client.responseStatus response) of
    201 -> pure ()
    _ -> do
      IO.hPrint IO.stderr response
      BSL.hPutStr IO.stderr (Client.responseBody response)
      fail "Failed to upload file to release!"

getUploadUrl
  :: Client.Manager
  -> String
  -> String
  -> String
  -> String
  -> IO Template.UriTemplate
getUploadUrl manager aToken anOwner aRepo aTag = do
  (Right json) <- getTag manager aToken anOwner aRepo aTag
  let (Just (Aeson.String text)) = HashMap.lookup (Text.pack "upload_url") json
  let uploadUrl = Text.unpack text
  let (Right template) = Template.parseTemplate uploadUrl
  pure template

getTag
  :: Client.Manager
  -> String
  -> String
  -> String
  -> String
  -> IO (Either String Aeson.Object)
getTag manager aToken anOwner aRepo aTag = do
  let format = "https://api.github.com/repos/%s/%s/releases/tags/%s"
  let url = Printf.printf format anOwner aRepo aTag
  initialRequest <- Client.parseUrlThrow url
  let request =
        initialRequest
        {Client.requestHeaders = [authorizationHeader aToken, userAgentHeader]}
  response <- Client.httpLbs request manager
  let body = Client.responseBody response
  let json = Aeson.eitherDecode body
  return json

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
  -> Template.UriTemplate
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
  -> Template.UriTemplate
  -> String
  -> Client.RequestBody
  -> String
  -> IO (Client.Response BSL.ByteString)
uploadBody manager template aToken body aName = do
  let url =
        Template.render
          template
          [("name", Template.WrappedValue (Template.Single aName))]
  initialRequest <- Client.parseUrlThrow url
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
