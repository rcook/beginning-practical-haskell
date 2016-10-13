{-# LANGUAGE RecordWildCards #-}

module ChildHealthData.Table
 ( Row (..)
 , Table (..)
 , pageTable
 ) where

import qualified Data.Map as M
import           ChildHealthData.Types

type Row = (Question, Answer, SubgroupCategory, SubgroupValue, P, L, U, N)
type Table = [Row]

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
pageTable ggMap ps = foldr (subcategoryRows ggMap) [] ps
