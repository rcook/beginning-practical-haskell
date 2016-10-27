{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ChildHealthData.Table
 ( Row (..)
 , Table (..)
 , pageTable
 ) where

import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.ICU.Translit
import           ChildHealthData.Types

asciiTrans :: Transliterator
asciiTrans = trans "Latin-ASCII"

asciiTransliterate :: T.Text -> T.Text
asciiTransliterate = transliterate asciiTrans

type Row = (Question, Answer, SubgroupCategory, SubgroupValue, P, L, U, N)
type Table = [Row]

transliterateRow :: Row -> Row
transliterateRow (q, a, sgc, sgv, p, l, u, n) =
  ( (Question . asciiTransliterate . unQuestion) q
  , (Answer . asciiTransliterate . unAnswer) a
  , (SubgroupCategory . asciiTransliterate . unSubgroupCategory) sgc
  , (SubgroupValue . asciiTransliterate . unSubgroupValue ) sgv
  , p
  , l
  , u
  , n
  )

answerRows :: SubgroupCategory -> SubgroupValue -> Question -> [(Answer, ValueTuple)] -> Table -> Table
answerRows sgc sgv question answers rs = foldr (helper sgc sgv question) rs answers
    where
        helper :: SubgroupCategory -> SubgroupValue -> Question -> (Answer, ValueTuple) -> Table -> Table
        helper sgc sgv question (answer, (p, (l, u), n)) rs = (question, answer, sgc, sgv, p, l, u, n) : rs

subcategoryRows :: GGMap -> Page -> Table -> Table
subcategoryRows ggMap Page{..} rs = foldr (\(sgv, xyzs) -> answerRows sgc sgv question (zip answers xyzs)) rs subcategories
    where
        Just sgc = M.lookup gg ggMap

pageTable :: GGMap -> [Page] -> Table
pageTable ggMap ps = map transliterateRow $ foldr (subcategoryRows ggMap) [] ps
