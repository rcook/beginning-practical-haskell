module ChildHealthData.Types
  ( Answer (..)
  , C (..)
  , GG (..)
  , GGMap (..)
  , HTML (..)
  , L (..)
  , N (..)
  , P (..)
  , Page (..)
  , QQ (..)
  , Question (..)
  , Subcategory (..)
  , SubgroupCategory (..)
  , SubgroupValue (..)
  , U (..)
  , ValueTuple (..)
  ) where

import qualified Data.Map as M
import qualified Data.Text as T

newtype HTML = HTML { unHTML :: T.Text } deriving Show

newtype QQ = QQ { unQQ :: Int } deriving (Eq, Ord, Show)
newtype GG = GG { unGG :: Int } deriving (Eq, Ord, Show)
newtype P = P { unP :: Double } deriving Show
newtype L = L { unL :: Double } deriving Show
newtype U = U { unU :: Double } deriving Show
type C = (L, U)
newtype N = N { unN :: Int } deriving Show
type ValueTuple = (P, C, N)
newtype Question = Question { unQuestion :: T.Text } deriving Show
newtype Answer = Answer { unAnswer :: T.Text } deriving Show
newtype SubgroupCategory = SubgroupCategory { unSubgroupCategory :: T.Text } deriving Show
newtype SubgroupValue = SubgroupValue { unSubgroupValue :: T.Text } deriving Show
type Subcategory = (SubgroupValue, [ValueTuple])

data Page = Page
  { qq :: QQ
  , gg :: GG
  , question :: Question
  , answers :: [Answer]
  , subcategories :: [Subcategory]
  } deriving Show

type GGMap = M.Map GG SubgroupCategory
