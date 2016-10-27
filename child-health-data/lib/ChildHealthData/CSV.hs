{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ChildHealthData.CSV
  ( encodeCSV
  ) where

import qualified Data.ByteString.Char8 as SC8
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Csv
import qualified Data.Text as T
import           Data.Text.ICU.Translit
import qualified Data.Vector as V

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

qHeader :: SC8.ByteString
qHeader = "question"

aHeader :: SC8.ByteString
aHeader = "answer"

sgcHeader :: SC8.ByteString
sgcHeader = "subgroup category"

sgvHeader :: SC8.ByteString
sgvHeader = "subgroup value"

pHeader :: SC8.ByteString
pHeader = "%"

lHeader :: SC8.ByteString
lHeader = "CI - lower"

uHeader :: SC8.ByteString
uHeader = "CI - upper"

nHeader :: SC8.ByteString
nHeader = "n"

rowHeaders :: Header
rowHeaders = V.fromList
  [ qHeader
  , aHeader
  , sgcHeader
  , sgvHeader
  , pHeader
  , lHeader
  , uHeader
  , nHeader
  ]

instance ToNamedRecord Row where
    toNamedRecord (Row q a sgc sgv p l u n) = namedRecord
      [ qHeader .= q
      , aHeader .= a
      , sgcHeader .= sgc
      , sgvHeader .= sgv
      , pHeader .= p
      , lHeader .= l
      , uHeader .= u
      , nHeader .= n
      ]

asciiTrans :: Transliterator
asciiTrans = trans "Latin-ASCII"

asciiTransliterate :: T.Text -> T.Text
asciiTransliterate = transliterate asciiTrans

transliterateRow :: Row -> Row
transliterateRow (Row q a sgc sgv p l u n) = Row
  ((Question . asciiTransliterate . unQuestion) q)
  ((Answer . asciiTransliterate . unAnswer) a)
  ((SubgroupCategory . asciiTransliterate . unSubgroupCategory) sgc)
  ((SubgroupValue . asciiTransliterate . unSubgroupValue ) sgv)
  p
  l
  u
  n

encodeCSV :: Table -> C8.ByteString
encodeCSV rs = encodeByName rowHeaders (map transliterateRow rs)
