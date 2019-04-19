module Trace.Hpc.CodecovJson where

import Trace.Hpc.Mix
import Trace.Hpc.Tix

import qualified Data.ByteString.Lazy as LBS

data CodecovJson

fromTixAndMixes :: Tix -> [Mix] -> CodecovJson
fromTixAndMixes tix mixes = undefined

toLBS :: CodecovJson -> LBS.ByteString
toLBS ccJson = undefined
