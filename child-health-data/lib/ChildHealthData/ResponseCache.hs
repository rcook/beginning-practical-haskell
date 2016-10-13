module ChildHealthData.ResponseCache
  ( fetchHTML
  ) where

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text.IO as T
import           Network.Wreq
import           System.Directory
import           System.FilePath
import           Text.Printf

import           ChildHealthData.Types

url :: QQ -> GG -> String
url qq gg =
    let qqValue = unQQ qq
        ggValue = unGG gg
    in
        if ggValue == -1
        then printf "http://childhealthdata.org/browse/survey/results?q=%d&r=1" qqValue
        else printf "http://childhealthdata.org/browse/survey/results?q=%d&r=1&g=%d" qqValue ggValue

responsePath :: FilePath -> QQ -> GG -> FilePath
responsePath dir qq gg = dir </> printf "_%d_%d" (unQQ qq) (unGG gg)

fetchHTML :: QQ -> GG -> IO HTML
fetchHTML qq gg = do
    let path = responsePath ".cache" qq gg
    d <- doesFileExist path
    unless d $ do
        let u = url qq gg
        r <- get u
        let c = r ^. responseBody
        C8.writeFile path c
    HTML <$> T.readFile path
