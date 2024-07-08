module Main (main) where

import Utils
import JsonUtils
import ApteParser (parserMain)
import ApteExpander (expanderMain, tabulate)
import qualified Data.List as L
import System.FilePath

main :: IO ()
main = do
  putStrLn "Parsing.."
  parserMain
  putStrLn "\nParsing complete.\nExpanding compounds.."
  es <- expanderMain
  putStrLn "\nExpanding complete.\nTabulating.."
  tabulate es
  putStrLn "\nDone."

