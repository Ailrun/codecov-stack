module Codecov.Api.Upload where

import Data.Maybe
import Data.List
import Numeric

import qualified Codecov.SupportedCis as SupportedCis

defaultOptions :: String -> Options
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

createBaseApiUrl :: ApiVersion -> String
createBaseApiUrl v = "https://codecov.io/upload/" <> show v

-- |
-- Do I need to check safeness of option values?
createQuery :: Options -> String
createQuery o
  = intercalate "&" queryParts
  where
    queryParts
      = map toQueryPart
        . mapMaybe sequence
        $ [ ("commit", Just (oCommit o))
          , ("token", oToken o)
          , ("branch", oBranch o)
          , ("build", fmap (flip showInt "") (oBuild o))
          , ("job", fmap (flip showInt "") (oJob o))
          , ("build_url", oBuildUrl o)
          , ("name", oName o)
          , ("slug", fmap show (oSlug o))
          , ("yaml", oYaml o)
          , ("service", fmap show (oService o))
          , ("flags", fmap showFlags (oFlags o))
          , ("pr", fmap (flip showInt "") (oPr o))
          ]

    toQueryPart (k, v) = k <> "=" <> v

createApiUrl :: ApiVersion -> Options -> String
createApiUrl v q = createBaseApiUrl v <> "?" <> createQuery q

data Options
  = Options
    { oCommit :: String
    , oToken :: Maybe String
    , oBranch :: Maybe String
    , oBuild :: Maybe Int
    , oJob :: Maybe Int
    , oBuildUrl :: Maybe String
    , oName :: Maybe String
    , oSlug :: Maybe Slug
    , oYaml :: Maybe String
    , oService :: Maybe SupportedCis.Type
    , oFlags :: Maybe Flags
    , oPr :: Maybe Int
    }
  deriving ( Eq
           , Show
           )

type Flags = [String]

showFlags :: Flags -> String
showFlags = intercalate ","

data Slug = Slug { sOwner :: String, sRepo :: String }
  deriving ( Eq
           )

instance Show Slug where
  show (Slug owner repo) = owner <> "/" <> repo

data ApiVersion
  = ApiV2
  | ApiV4
  | ApiV5
  deriving ( Eq
           )

instance Show ApiVersion where
  show ApiV2 = "v2"
  show ApiV4 = "v4"
  show ApiV5 = "v5"
