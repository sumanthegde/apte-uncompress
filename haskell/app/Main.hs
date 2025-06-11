module Main (main) where

import Utils
import JsonUtils
import ApteParser (parserMain)
import ApteExpander (expanderMain, tabulate, shardAndStore, koshaFormShardAndStore)
import qualified Data.List as L
import System.FilePath

main :: IO ()
main = do
  putStrLn "Parsing.."
  parserMain
  putStrLn "\nParsing complete.\nExpanding compounds.."
  es <- expanderMain
  putStrLn "\nExpanding complete."
  putStrLn "\nSharding.."
  shardAndStore es
  putStrLn "\nSharding (kosha format).."
  koshaFormShardAndStore es
  putStrLn "Tabulating.."
  tabulate es
  putStrLn "\nDone."

