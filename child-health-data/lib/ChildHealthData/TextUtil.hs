module ChildHealthData.TextUtil
  ( splitOnOnce
  , stripOptionalPrefix
  , stripOptionalSuffix
  , unsafeDouble
  , unsafeInt
  , unsafeSignedInt
  ) where

import           Data.Strings
import qualified Data.Text as T
import qualified Data.Text.Read as T

splitOnOnce :: T.Text -> T.Text -> (T.Text, T.Text)
splitOnOnce delimiter s = let (l, r) = T.breakOn delimiter s in (l, T.drop (T.length delimiter) r)

stripOptionalPrefix :: String -> T.Text -> T.Text
stripOptionalPrefix prefix s =
    if strStartsWith s prefix
    then T.drop (length prefix) s
    else s

stripOptionalSuffix :: String -> T.Text -> T.Text
stripOptionalSuffix suffix s =
    if strEndsWith s suffix
    then T.dropEnd (length suffix) s
    else s

unsafeDouble :: T.Text -> Double
unsafeDouble s = let Right (x, _) = T.double s in x

unsafeInt :: T.Text -> Int
unsafeInt s = let Right (x, _) = T.decimal s in x

unsafeSignedInt :: T.Text -> Int
unsafeSignedInt s = let Right (x, _) = T.signed T.decimal s in x
