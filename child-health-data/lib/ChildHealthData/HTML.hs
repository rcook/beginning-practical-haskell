{-# LANGUAGE OverloadedStrings #-}

module ChildHealthData.HTML
  ( elementsWithAttr
  , elementsWithClass
  , elementsWithId
  , innerText
  ) where

import           Control.Lens hiding (children)
import qualified Data.Text as T
import           Text.Taggy.Lens

elementsWithAttr :: Element -> T.Text -> T.Text -> T.Text -> [Element]
elementsWithAttr e name attrName attrValue = e ^.. allNamed (only name) . attributed (ix attrName . only attrValue)

elementsWithClass :: Element -> T.Text -> T.Text -> [Element]
elementsWithClass e name class_ = elementsWithAttr e name "class" class_

elementsWithId :: Element -> T.Text -> T.Text -> [Element]
elementsWithId e name id_ = elementsWithAttr e name "id" id_

innerText :: Element -> T.Text
innerText e = foldr (T.append . innerTextHelper) T.empty (e ^. children)
    where
        innerTextHelper :: Node -> T.Text
        innerTextHelper n =
            case n of NodeElement e -> innerText e
                      NodeContent t -> t
