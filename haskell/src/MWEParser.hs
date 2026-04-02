module MWEParser where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU8
import System.FilePath
import System.Directory
import System.IO
import Control.Monad
import qualified Data.List.Split as LS
import qualified Data.List as L
import Text.ParserCombinators.ReadP as R
import Data.Char
import Data.Maybe
import JsonUtils
import SqliteUtil

-- | Parse a balanced parenthesis block and return nothing
parseParenBlock :: R.ReadP ()
parseParenBlock = do
    void $ R.char '('
    skipBalancedParens

-- | Recursively skip contents until the matching closing parenthesis
skipBalancedParens :: R.ReadP ()
skipBalancedParens = do
    R.skipMany (parseParenBlock R.<++ void (R.satisfy (`notElem` "()")))
    void $ R.char ')'

-- | Parse a Devanagari block {#...#} and extract words separated by comma or semicolon
parseDevBlock :: R.ReadP [String]
parseDevBlock = do
    void $ R.string "{#"
    content <- R.manyTill R.get (R.string "#}")
    let trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
        isSLP1 c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '~' || c == '\''
        process word = 
            let tWord = trim word
            in if "-" `L.isPrefixOf` tWord 
               then Nothing 
               else Just (takeWhile isSLP1 tWord)
        words = mapMaybe process $ LS.splitOneOf ",;" content
    return $ filter (not . null) words

-- | Parse the body of the entry: skip () blocks, extract from {# #} blocks, ignore other chars
parseBody :: R.ReadP [String]
parseBody = fmap concat $ R.many $
    (parseParenBlock >> return [])
    R.<++ parseDevBlock
    R.<++ (R.get >> return [])

-- | Parse the headword from the starting string '<k1>headword<k2>'
parseHeadword :: R.ReadP String
parseHeadword = do
    _ <- R.manyTill R.get (R.string "<k1>")
    R.manyTill R.get (R.string "<k2>")

-- | An entry consists of a headword and the body
parseEntry :: R.ReadP (String, [String])
parseEntry = do
    hw <- parseHeadword
    devWords <- parseBody
    return (hw, devWords)

-- | Run the parser on a single entry string (between <L> and <LEND>)
--   We use R.readP_to_S and return the result that consumes the most input.
runParseEntry :: String -> Maybe (String, [String])
runParseEntry str =
    case R.readP_to_S (parseEntry <* R.eof) str of
        [] -> 
            -- fallback if R.eof is too strict: just find the maximal parse
            case R.readP_to_S parseEntry str of
                [] -> Nothing
                parses -> Just $ fst $ last parses
        parses -> Just $ fst $ last parses

-- | Main extraction function for the entire string content of mwe.txt
--   Provides a list of mappings from headword -> [devanagari words]
parseMWE :: String -> [(String, [String])]
parseMWE content =
    let entries = filter (not . null) $ LS.splitOn "\n<LEND>" content
        -- A valid entry block will start with <L> (possibly preceded by tabs/newlines which splitOn leaves)
        trimEntry e = case LS.splitOn "<L>" e of
            (x:xs) | not (null xs) -> last xs
            _ -> e
        validEntries = filter (\e -> "<k1>" `L.isInfixOf` e) (map trimEntry entries)
    in mapMaybe runParseEntry validEntries

-- | Process the MWE text file and return the list of tuples
--   Input is mwe.txt from Cologne database
processMWEFile :: FilePath -> FilePath -> IO [(String, [String])]
processMWEFile inPath outPath = do
    contents <- readFile inPath
    let mwes = parseMWE contents
    store outPath mwes
    print $ "Stored MW E-S entries into " ++ outPath
    loadMWES mwes
    print $ "Loaded MW E-S entries into sqlite"
    return mwes