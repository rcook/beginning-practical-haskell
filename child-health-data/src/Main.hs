module Main (main, testFunction) where

import           Control.Monad (forM_)
import qualified System.Environment as Env

-- | Minimal function including doctest
-- Examples:
-- >>> testFunction "string"
-- "[[string]]"
testFunction :: String -> String
testFunction s = "[[" ++ s ++ "]]"

main :: IO ()
main = do
    putStrLn "child-health-data"
    args <- Env.getArgs
    forM_ args $ \arg -> putStrLn $ "arg: " ++ arg