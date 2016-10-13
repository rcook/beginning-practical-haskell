{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           ChildHealthData.CSV
import           ChildHealthData.ResponseCache
import           ChildHealthData.Scrape
import           ChildHealthData.Table
import           ChildHealthData.TextUtil
import           ChildHealthData.Types
import           ChildHealthData.Util

readQQFile :: FilePath -> IO [QQ]
readQQFile path = do
    ls <- T.lines <$> T.readFile path
    return $ map (QQ . unsafeInt) ls

readGGFile :: FilePath -> IO ([GG], GGMap)
readGGFile path = do
    ls <- T.lines <$> T.readFile path
    let entries = map parseGGEntries ls
    return $ (map fst entries, M.fromList entries)
    where
        parseGGEntries :: T.Text -> (GG, SubgroupCategory)
        parseGGEntries l = let (ggStr, categoryStr) = splitOnOnce " " l in ((GG . unsafeSignedInt) ggStr, SubgroupCategory categoryStr)

readPages :: [QQ] -> [GG] -> IO [Page]
readPages qqs ggs = accumulateM [] [(qq, gg) | qq <- qqs, gg <- ggs] $ \ps (qq, gg) -> do
    c <- fetchHTML qq gg
    return $ if T.length (unHTML c) > 0
                then parsePage qq gg c : ps
                else ps

main :: IO ()
main = do
    qqs <- readQQFile "qqs.txt"
    (ggs, ggMap) <- readGGFile "ggs.txt"

    pages <- readPages qqs ggs

    let t = pageTable ggMap pages
    C8.putStrLn $ encodeCSV t
