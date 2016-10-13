{-# LANGUAGE OverloadedStrings #-}

module ChildHealthData.Scrape
  ( Page (..)
  , parsePage
  ) where

import           Control.Lens
import           Data.List.Extra
import           Data.String.Conv
import           Data.Strings
import qualified Data.Text as T
import           Text.Taggy
import           Text.Taggy.Lens

import           ChildHealthData.HTML
import           ChildHealthData.TextUtil
import           ChildHealthData.Types

tableId :: T.Text
tableId = "PageContent_PageContent_C001_tblResults"

parsePage :: QQ -> GG -> HTML -> Page
parsePage qq gg c =
    let tables = (toS (unHTML c)) ^.. html . allNamed (only "table") . attributed (ix "id" . only tableId)
        table = head tables
        caption = head $ table ^.. allNamed (only "caption")
        thead = head $ table ^.. allNamed (only "thead")
        tr = head $ thead ^.. allNamed (only "tr")
        ths = dropEnd 1 (elementsWithClass tr "th" "result")
        q = Question $ parseQuestion caption
        as = map parseAnswer ths
        tbody = head $ table ^.. allNamed (only "tbody")
        trs = tbody ^.. allNamed (only "tr")
        s = parseSubcategories trs
    in Page qq gg q as s

parseSubcategories :: [Element] -> [Subcategory]
parseSubcategories [] = []
parseSubcategories (tr0 : tr1 : tr2 : tr3 : trs) = (value, answers) : parseSubcategories trs
    where
        value :: SubgroupValue
        value = SubgroupValue (case elementsWithClass tr0 "th" "compare" of e : _ -> innerText e
                                                                            [] -> "all")
        answers :: [ValueTuple]
        answers = zip3 ps cs ns
        ps :: [P]
        ps = map (parseP . innerText) (elementsWithClass tr0 "span" "type_p")
        parseP :: T.Text -> P
        parseP = P . unsafeDouble
        cs :: [C]
        cs = map (parseC . innerText) (elementsWithClass tr1 "span" "type_c")
        parseC :: T.Text -> C
        parseC s =
            let (l, u) = strSplit "-" $ (stripOptionalPrefix "(" . stripOptionalSuffix ")") s
            in (L . unsafeDouble $ T.strip l, U . unsafeDouble $ T.strip u)
        ns :: [N]
        ns = map (parseN . innerText) (elementsWithClass tr2 "span" "type_n")
        parseN :: T.Text -> N
        parseN = N . unsafeInt . T.replace "," T.empty

parseQuestion :: Element -> T.Text
parseQuestion = (stripOptionalSuffix " (details)") . T.strip . innerText

parseAnswer :: Element -> Answer
parseAnswer = Answer . T.strip . innerText
