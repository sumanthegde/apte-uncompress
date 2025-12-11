{-# LANGUAGE OverloadedStrings #-}

module SqliteUtilKritDarsh (bulkLoadFromTSVKritDarsh) where 

import Database.SQLite.Simple
import qualified Utils as U
import System.FilePath ((</>))
import GHC.Int (Int64)
import Control.Monad

-- | Path to the SQLite database file.
databaseFile :: String
databaseFile = U.apteOutput </> "apte_data_kritdarsh.db"

bulkLoadFromTSVKritDarsh :: [(Integer, String)] -> [(Integer,String)] -> IO ()
bulkLoadFromTSVKritDarsh wordTable objTable = do
  conn <- open databaseFile
  -- Clear existing data
  execute_ conn "DROP TABLE IF EXISTS words"
  execute_ conn "DROP TABLE IF EXISTS objects"
  execute_ conn "CREATE TABLE IF NOT EXISTS words (id INTEGER, word TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS objects (word_id INTEGER PRIMARY KEY, object TEXT, FOREIGN KEY (word_id) REFERENCES word(id))"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_words_word ON words (word);"
  execute_ conn "BEGIN TRANSACTION"

  -- Insert data using executeMany
  executeMany conn "INSERT INTO words (id, word) VALUES (?, ?)" wordTable
  executeMany conn "INSERT INTO objects (word_id, object) VALUES (?, ?)" objTable
  execute_ conn "COMMIT"

  close conn
  putStrLn $ "Successfully bulk loaded data into sqlite db using executeMany."