{-# LANGUAGE OverloadedStrings #-}
module Codecov.Api.Upload where

import Data.Aeson ( FromJSON, Value )
import Data.Function
import Data.Maybe
import Data.String ( IsString, fromString )
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Numeric

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Codecov.SupportedCis as SupportedCis

send :: ApiVersion -> Options -> BSL.ByteString -> IO (Response Value)
send ApiV2 options report = httpJSON request
  where
    request
      = apiUrl
        & parseRequest_
        & setRequestQueryString query
        & setRequestMethod "POST"
        & setRequestHeaders headers
        & setRequestBodyLBS report

    apiUrl = createApiUrl ApiV2
    query = createQuery options
    headers
      = [ ("Accept", "application/json")
        ]
send _ _ _ = error "Codecov.Api.Upload.send: Other versions than v2 are not implemented yet."

defaultOptions :: BS.ByteString -> Options
defaultOptions commit
  = Options
    { oCommit = commit
    , oToken = Nothing
    , oBranch = Nothing
    , oBuild = Nothing
    , oJob = Nothing
    , oBuildUrl = Nothing
    , oName = Nothing
    , oSlug = Nothing
    , oYaml = Nothing
    , oService = Nothing
    , oFlags = Nothing
    , oPr = Nothing
    }

createApiUrl :: ApiVersion -> String
createApiUrl v = "https://codecov.io/upload/" <> apiVersionToByteString v

-- |
-- Do I need to check safeness of option values?
createQuery :: Options -> Query
createQuery o
  = fmap (fmap Just)
    . mapMaybe sequence
    $
    [ ("commit", Just (oCommit o))
    , ("token", oToken o)
    , ("branch", oBranch o)
    , ("build", fmap toStringLike (oBuild o))
    , ("job", fmap toStringLike (oJob o))
    , ("build_url", oBuildUrl o)
    , ("name", oName o)
    , ("slug", fmap toStringLike (oSlug o))
    , ("yaml", oYaml o)
    , ("service", fmap toStringLike (oService o))
    , ("flags", fmap flagsToByteString (oFlags o))
    , ("pr", fmap toStringLike (oPr o))
    ]

toStringLike :: (Show a, IsString b) => a -> b
toStringLike = fromString . show

data Options
  = Options
    { oCommit :: BS.ByteString
    , oToken :: Maybe BS.ByteString
    , oBranch :: Maybe BS.ByteString
    , oBuild :: Maybe Int
    , oJob :: Maybe Int
    , oBuildUrl :: Maybe BS.ByteString
    , oName :: Maybe BS.ByteString
    , oSlug :: Maybe Slug
    , oYaml :: Maybe BS.ByteString
    , oService :: Maybe SupportedCis.Type
    , oFlags :: Maybe Flags
    , oPr :: Maybe Int
    }
  deriving ( Eq
           , Show
           )

type Flags = [BS.ByteString]

flagsToByteString :: Flags -> BS.ByteString
flagsToByteString = BS.intercalate ","

data Slug = Slug { sOwner :: BS.ByteString, sRepo :: BS.ByteString }
  deriving ( Eq
           , Show
           )

data ApiVersion
  = ApiV2
  | ApiV4
  | ApiV5
  deriving ( Eq
           , Show
           )

apiVersionToByteString :: ApiVersion -> String
apiVersionToByteString ApiV2 = "v2"
apiVersionToByteString ApiV4 = "v4"
apiVersionToByteString ApiV5 = "v5"
