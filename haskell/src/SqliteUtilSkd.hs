{-# LANGUAGE OverloadedStrings #-}

module SqliteUtilSkd where

import Database.SQLite.Simple
import qualified Utils as U
import System.FilePath ((</>))
import Control.Monad

type S = String
-- | Bulk loads SKD data into the skd table.
-- Input: rows is a list of lists, each inner list should have 6 columns:
-- [headword, gender, intro, upasarga, dhatu, pratyaya, shesha]
bulkLoadFromTsvSkd :: String -> [(S, S, S, S, S, S, S, S)] -> IO ()
bulkLoadFromTsvSkd dbPath rows = do
  conn <- open dbPath
  
  -- Drop existing table and indexes
  execute_ conn "DROP TABLE IF EXISTS skd"
  execute_ conn "DROP INDEX IF EXISTS idx_skd_headword"
  
  -- Create table with specified columns
  execute_ conn "CREATE TABLE IF NOT EXISTS skd (headword TEXT, gender TEXT, intro TEXT, upasarga TEXT, dhatu TEXT, pratyaya TEXT, shesha TEXT, headword_og TEXT)"
  
  -- Create index on headword column
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_skd_headword ON skd (headword)"
  
  -- Bulk insert data
  execute_ conn "BEGIN TRANSACTION"
  executeMany conn "INSERT INTO skd (headword, gender, intro, upasarga, dhatu, pratyaya, shesha, headword_og) VALUES (?, ?, ?, ?, ?, ?, ?, ?)" rows
  execute_ conn "COMMIT"
  
  close conn
  putStrLn $ "Successfully bulk loaded " ++ show (length rows) ++ " rows into SKD sqlite db"