{-# LANGUAGE RecordWildCards #-}

module ChildHealthData.CSV
  ( encodeCSV
  ) where

import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Csv

import           ChildHealthData.Table
import           ChildHealthData.Types

instance ToField GG where
    toField = toField . unGG

instance ToField P where
    toField = toField . unP

instance ToField L where
    toField = toField . unL

instance ToField U where
    toField = toField . unU

instance ToField N where
    toField = toField . unN

instance ToField Question where
    toField = toField . unQuestion

instance ToField Answer where
    toField = toField . unAnswer

instance ToField SubgroupCategory where
    toField = toField . unSubgroupCategory

instance ToField SubgroupValue where
    toField = toField . unSubgroupValue

encodeCSV :: Table -> C8.ByteString
encodeCSV = encode
