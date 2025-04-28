module JsonUtils (
  -- * Takes a transformer function, runs it on json obtained from one file, dumps output in another file
  --   Useful when jsons have unicode characters that the normal print handles poorly 
  tweakFileIO
  -- * Same as tweakFileIO but out-file is stdout
  , tweakIO
  -- * Helper functions. Not sure if to expose.
  , load
  , store
)

where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A

import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy (fromStrict)
import Data.Aeson.Encode.Pretty (encodePretty)

load :: A.FromJSON a => FilePath -> a -> IO a
load fp defaultInput = do
  content <- BS.readFile fp
  let x = A.decode $ fromStrict content
  return $ fromMaybe defaultInput x

store :: A.ToJSON a => FilePath -> a -> IO ()
store fp obj = BL.writeFile fp (encodePretty obj)

tweakIO :: (A.FromJSON t, A.ToJSON a) => (t -> a) -> t -> FilePath -> IO ()
tweakIO tweaker defaultInput inPath = do
  x <- load inPath defaultInput
  BL.putStr $ encodePretty $ tweaker x
  putStrLn ""

tweakFileIO :: (A.FromJSON t, A.ToJSON a) => (t -> a) -> t -> FilePath -> FilePath -> IO ()
tweakFileIO tweaker defaultInput inPath outPath = do
  x <- load inPath defaultInput
  store outPath (tweaker x)
  


