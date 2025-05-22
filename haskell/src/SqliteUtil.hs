{-# LANGUAGE OverloadedStrings #-}

module SqliteUtil where

import Database.SQLite.Simple
import qualified Utils as U
import System.FilePath ((</>))
import GHC.Int (Int64)
import Control.Monad

-- | Path to the SQLite database file.
databaseFile :: String
databaseFile = U.apteOutput </> "apte_data.sqlite"

-- | Creates the SQLite database and the necessary tables if they don't exist.
-- createDatabase :: IO ()
-- createDatabase = do
--   conn <- open databaseFile
--   -- If the table doesn't exist, the DELETE statement will fail,
--   -- but execute_ handles this gracefully without stopping the program.
--   execute_ conn "DELETE FROM meanings"
--   execute_ conn "DELETE FROM metadata"
--   execute_ conn "CREATE TABLE IF NOT EXISTS metadata (id INTEGER PRIMARY KEY AUTOINCREMENT, ancestry TEXT, expanded_banner TEXT)"
--   execute_ conn "CREATE TABLE IF NOT EXISTS meanings (meta_id INTEGER, meaning TEXT, FOREIGN KEY (meta_id) REFERENCES metadata(id))"
--   execute_ conn "CREATE INDEX idx_metadata_ancestry ON metadata (ancestry);"
--   execute_ conn "CREATE INDEX idx_meanings_meta_id ON meanings (meta_id);"
--   close conn
--   putStrLn $ "Database '" ++ databaseFile ++ "' and tables 'metadata' and 'meanings' created successfully (if they didn't exist)."

-- -- | Adds a record to the metadata table using an existing connection and returns the generated ID.
-- addMetadataRecord :: Connection -> String -> String -> IO Int64
-- addMetadataRecord conn ancestry expandedBanner = do
--   execute conn "INSERT INTO metadata (ancestry, expanded_banner) VALUES (?, ?)" (ancestry, expandedBanner)
--   lastInsertRowId conn

-- -- | Adds a record to the meanings table using an existing connection.
-- addMeaningRecord :: Connection -> Integer -> String -> IO ()
-- addMeaningRecord conn metaId meaning = do
--   execute conn "INSERT INTO meanings (meta_id, meaning) VALUES (?, ?)" (metaId, meaning)

-- persistLocBannerMeanings :: [(String, String, [String])] -> IO ()
-- persistLocBannerMeanings lbms = do
--   conn <- open databaseFile
--   -- If the table doesn't exist, the DELETE statement will fail,
--   -- but execute_ handles this gracefully without stopping the program.
--   execute_ conn "DELETE FROM meanings"
--   execute_ conn "DELETE FROM metadata"
--   execute_ conn "CREATE TABLE IF NOT EXISTS metadata (id INTEGER PRIMARY KEY AUTOINCREMENT, ancestry TEXT, expanded_banner TEXT)"
--   execute_ conn "CREATE TABLE IF NOT EXISTS meanings (meta_id INTEGER, meaning TEXT, FOREIGN KEY (meta_id) REFERENCES metadata(id))"
--   execute_ conn "CREATE INDEX idx_metadata_ancestry ON metadata (ancestry);"
--   execute_ conn "CREATE INDEX idx_meanings_meta_id ON meanings (meta_id);"
--   forM_ lbms $ \(locStr,bannerExpStr,meaningStrs) -> do
--     execute conn "INSERT INTO metadata (ancestry, expanded_banner) VALUES (?, ?)" (locStr, bannerExpStr)
--     i <- lastInsertRowId conn
--     forM_ meaningStrs $ \meaningStr -> do
--       execute conn "INSERT INTO meanings (meta_id, meaning) VALUES (?, ?)" (i, meaningStr)


-- | Bulk loads data from TSV files into the metadata and meanings tables.
-- | Bulk loads data from TSV files into the metadata and meanings tables.
-- | Bulk loads data into the metadata and meanings tables using executeMany.
bulkLoadFromTSV :: [(Integer, String, String)] -> [(Integer, String)] -> IO ()
bulkLoadFromTSV metadataRows meaningsRows = do
  conn <- open databaseFile
  -- Clear existing data
  execute_ conn "DELETE FROM meanings"
  execute_ conn "DELETE FROM metadata"
  execute_ conn "CREATE TABLE IF NOT EXISTS metadata (id INTEGER PRIMARY KEY AUTOINCREMENT, ancestry TEXT, expanded_banner TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS meanings (meta_id INTEGER, meaning TEXT, FOREIGN KEY (meta_id) REFERENCES metadata(id))"
  execute_ conn "CREATE INDEX idx_metadata_ancestry ON metadata (ancestry);"
  execute_ conn "CREATE INDEX idx_meanings_meta_id ON meanings (meta_id);"
  execute_ conn "BEGIN TRANSACTION"

  -- Insert data using executeMany
  executeMany conn "INSERT INTO metadata (id, ancestry, expanded_banner) VALUES (?, ?, ?)" metadataRows
  executeMany conn "INSERT INTO meanings (meta_id, meaning) VALUES (?, ?)" meaningsRows

  execute_ conn "COMMIT"
  close conn
  putStrLn $ "Successfully bulk loaded data into sqlite db using executeMany."