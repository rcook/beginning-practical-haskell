module Main (main) where

import qualified System.FilePath.Glob as Glob
import qualified Test.DocTest as DocTest

includeDirs :: [String]
includeDirs = []

doctestWithIncludeDirs :: [String] -> IO ()
doctestWithIncludeDirs fs = DocTest.doctest (map ("-I" ++) includeDirs ++ fs)

main :: IO ()
main = Glob.glob "src/**/*.hs" >>= doctestWithIncludeDirs