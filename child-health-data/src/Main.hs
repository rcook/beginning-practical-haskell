{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Options.Applicative

import           ChildHealthData.CSV
import           ChildHealthData.ResponseCache
import           ChildHealthData.Scrape
import           ChildHealthData.Table
import           ChildHealthData.TextUtil
import           ChildHealthData.Types
import           ChildHealthData.Util
import           Paths_child_health_data

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

data Format = CSV | XLSX deriving Show

instance Read Format where
    readsPrec _ "csv" = [(CSV, "")]
    readsPrec _ "xlsx" = [(XLSX, "")]
    readsPrec _ _ = []

data Settings = Settings { format :: Format }

settings :: Parser Settings
settings =
    Settings <$> option auto
                  ( long "format"
                  <> short 'f'
                  <> metavar "FORMAT"
                  <> help "output format")

doIt :: Settings -> IO ()
doIt Settings{..} = do
    putStrLn $ "format=" ++ show format

main :: IO ()
main = execParser opts >>= doIt
    where
        opts = info
            (helper <*> settings)
            (fullDesc <> progDesc "Scrape child health data" <> header "child-health-data-app - scrape web pages")
    {-
    qqPath <- getDataFileName "qqs.txt"
    ggPath <- getDataFileName "ggs.txt"

    qqs <- readQQFile qqPath
    (ggs, ggMap) <- readGGFile ggPath

    pages <- readPages qqs ggs

    let t = pageTable ggMap pages
    C8.putStrLn $ encodeCSV t
    -}
