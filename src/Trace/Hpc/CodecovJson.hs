module Trace.Hpc.CodecovJson where

import Data.Maybe
import Trace.Hpc.Mix
import Trace.Hpc.Tix

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.Map as Map

type CodecovJson = Map.Map String CodecovFileJson

type CodecovFileJson = Map.Map Integer CodecovLineJson

data CodecovLineJson
  = FullyCovered Int
  | PartiallyCovered Int Int
  deriving ( Eq
           , Show
           )

fromTixAndMixes :: Tix -> [Mix] -> CodecovJson
fromTixAndMixes (Tix tixModules) mixes
  = Map.fromListWith mergeCodecovFileJsons namedCodecovFileJsons
  where
    namedCodecovFileJsons = fmap makeNamedCodecovFileJson tixModules

    makeNamedCodecovFileJson tixModule = (moduleName, codecovFileJson)
      where
        codecovFileJson = fromTixModuleAndMix tixModule mix
        mix = findMixForModuleName moduleName
        moduleName = tixModuleName tixModule

    findMixForModuleName moduleName
      = fromMaybe
        (error (".mix files for module " <> moduleName <> " is missing"))
        (L.find (\(Mix mn _ _ _ _) -> mn == moduleName) mixes)

-- |
-- 1. 'BinBox _ True' and 'BinBox _ False' should be handled together.
-- 2. 'ExpBox True' is for an expression whose execution depends on conditional structure.
fromTixModuleAndMix :: TixModule -> Mix -> CodecovFileJson
fromTixModuleAndMix (TixModule _ _ _ tixs) (Mix _ _ _ _ entries)
  = undefined . L.groupOn (fst . fst) $ zip entries tixs

mergeCodecovFileJsons :: CodecovFileJson -> CodecovFileJson -> CodecovFileJson
mergeCodecovFileJsons json1 json2 = undefined

toLBS :: CodecovJson -> LBS.ByteString
toLBS ccJson = undefined
