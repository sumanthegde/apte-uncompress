module Shabdakalpadruma where



import Utils
import Sandhi
import ParserCombUtils
-- import Natva
import JsonUtils
import SEncode
import Natva
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List.Split as LS
import qualified Data.List as L
import Text.ParserCombinators.ReadP as R
import Control.Lens ( (&), (^..), (^.), _Just, _Right )
import Data.Either
import Control.Monad
import System.FilePath
import System.Directory
import System.Environment
import System.IO
import Control.Monad.Trans.Except
import Data.Function
import Data.Char
import Data.Bifunctor
import Data.Functor
import Control.Applicative
import Data.Aeson (object, toJSON, Value)
import Data.Aeson.Key (fromString)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL
import qualified Data.String as DS -- (fromString)
import SqliteUtil
import qualified GHC.Unicode as R
import Data.Ord (comparing)
import Data.ByteString (dropEnd)
import Data.Bits (Bits(xor))
import Pratyayas (upasargas)


dataPath = "shabdakalpadruma.data"
dhatuPath = dataPath </> "dhatu.hsv"
skdPath = dataPath </> "skd2.json"
skdOut = dataPath </> "skd2.out.json"
tsvOut = dataPath </> "skd3.out.tsv"


prefixDhatuOverlap = ["कु"]
upasargasAll = reverse $ L.sortOn length $ upasargas ++ ["आ","निर्","दुर्","सं"] ++ prefixDhatuOverlap
-- MORE PARSER UTILS 
oneWhiteSpace = satisfy (`elem` " \n")
s_ :: ReadP String -> ReadP String
s_ p = pJoin [manyGreedy oneWhiteSpace, p]
s1_ p = pJoin [manyGreedy1 oneWhiteSpace, p]
s1'_ p = pJoin [p, manyGreedy1 oneWhiteSpace]

repeatMaximal :: ReadP String -> ReadP String
repeatMaximal = fmap concat . chainMaximal . repeat

munchUntil :: (Char -> Bool) -> ReadP String
munchUntil condition  = do
  thing <- munch (not.condition)
  munch condition
  return thing

postSkip :: ReadP b -> ReadP a -> ReadP b
postSkip toKeep toSkip = do
  k <- toKeep
  void toSkip <++ pure ()
  return k

prefixOpt :: ReadP String -> ReadP String -> ReadP String
prefixOpt p q = pJoin [p,q] <++ q

fromTo open close = (open ++) <$> surrLazy open close <&> (++ close)

isDevanagari :: Char -> Bool
isDevanagari c = 'ऀ' <= c && c <= 'ॿ' -- U+0900 - U+097F

devanagariOrParen :: ReadP String
devanagariOrParen = R.munch (isDevanagari `or2` (`elem` "()"))

devanagariUntilVirama :: String -> ReadP String
devanagariUntilVirama allowedNonDevaNagari = R.munch1 (\c -> isDevanagari c && c `notElem` "।॥" || c `elem` allowedNonDevaNagari)

-- | Space-separated words or blocks until a virama sign (। or ॥). We don't consume the virama sign.
--   A block can be either a string enclosed by “” (containing anything within it),
--   or it can denote the sutra number. Examples: ८ । ४ । ६८ ।  or  उणा० ४ । १०५ ।
--   Example: “अन्येभ्यो ऽपि दृश्यन्ते ।” उणा० ४ । १०५ । इति इत्वन् । is one sentence (The last '।' is not consumed)
devanagariSentence :: String -> ReadP String
devanagariSentence allowedNonDevaNagari = do
  let block = s_ $ fromTo "“" "”"
      --sutraNum = s_ $ pJoin [pSankhya <++ s_ (lit "उणां"), pVirama, pSankhya, pVirama, pSankhya<++ s_ (lit "उणां"), pVirama]
  repeatMaximal (paniniSutra <++ block <++ s_ (devanagariUntilVirama allowedNonDevaNagari))
 

virama :: ReadP String
virama = s_ $ lit "।" <++ lit "॥"

anyWord :: ReadP String
anyWord = R.munch1 (not.isSpace)

ignore :: ReadP String -> ReadP ()
ignore p = void (p R.<++ pure "")

maybeP :: ReadP a -> ReadP (Maybe a)
maybeP p = (Just <$> p) R.<++ pure Nothing

comSpaceDelimit :: ReadP ()
comSpaceDelimit =   ignore (lit ",") >> oneWhiteSpace >> R.skipSpaces

csd :: ReadP a -> ReadP a
csd p = comSpaceDelimit >> p

gender :: ReadP String
gender = let pum = lit "पुं" <++ (lit "पु" $> "पुं")
  in concat <$> chainMaximal (repeat $ csd (pum <++ lit "स्त्री" <++ lit "क्ली" <++ lit "त्रि" <++ lit "व्य"))

upasargaPluss :: ReadP String
upasargaPluss = let
--  ups = upasargas ++ ["आ","निर्","दुर्"] 
  pUp = L.foldl1' (R.<++) (lit <$> upasargasAll)
  in concat <$> chainMaximal (repeat (pJoin [s_ pUp, s_ (lit "+")]))

phalakas :: ReadP ()
phalakas = let
  single cs = case cs of [c] -> True; _ -> False;
  monosyl = guard' (single . filter (`elem` vowelsReal)) $ canon <$> R.munch1 isDevanagari
  in void $ chainMaximal $ repeat $ s1'_ monosyl

dhatuLookup :: String -> [[String]] -> ReadP String
dhatuLookup x = go where
  go [] = pfail
  go ([dhatu,aupadeshika,_meaning]:rest)
    | x == dhatu || x ++ "्" == dhatu = pure aupadeshika
    | x == aupadeshika || x ++ "ँ" == aupadeshika = pure aupadeshika
    | otherwise = go rest

dhatuOptWordLookPlus :: [[String]] -> ReadP String
dhatuOptWordLookPlus dhaList = do
  candid <- skipSpaces >> R.munch1 isDevanagari
  aupadeshikaDhatu <- dhatuLookup candid dhaList
  skipSpaces >> phalakas
  _dhaMeaning <- s_ (R.munch isDevanagari) <++ pure ""
  aheadSatisfy (("+" `L.isPrefixOf`) . dropWhile isSpace)
  return $ aupadeshikaDhatu -- ++ " " ++ dhaMeaning

pSankhya = s_ $ R.munch1 (`elem` "०१२३४५६७८९")
pVirama = s_ (lit "।")

paniniSutra :: ReadP String
paniniSutra = let 
  una = pJoin [s_ $ lit "उणां" <++ lit "उणा०", virama <++ pure ""]
  pSV = pJoin [pSankhya, pVirama]
  ashta = pJoin [pSV, pSV, pSV]
  unadi = pJoin [una, pSV, pSV] <++ pJoin [pSV, pSV, una]
  in s_ $ pJoin [fromTo "“" "”", ashta <++ unadi]

plusPratyayas :: ReadP String
plusPratyayas = pJoin [repeatMaximal (pJoin [s_ (lit "+"), devanagariSentence " ,"]), virama] -- pJoin [concat <$> chainMaximal (repeat (pJoin [s_ (lit "+"), prefixOpt paniniSutra (s_ devanagariUntilVirama)])), virama]

upasarDhatuPrat :: [[String]] -> ReadP [String]
upasarDhatuPrat dhaList = do
  lit "("
  intro <- guard' (not . L.isPrefixOf "यथा,") $ repeatMaximal $ pJoin [devanagariSentence " ,", virama]
  u <- upasargaPluss
  d <- dhatuOptWordLookPlus dhaList
  p <- plusPratyayas
  rest <- repeatMaximal $ pJoin [devanagariSentence " ,", virama]
  lit ")"
  return [intro,u,d,p,rest]  

derivationWithDhatuOrNot :: [[String]] -> ReadP [String]
derivationWithDhatuOrNot dhaList = upasarDhatuPrat dhaList <|> fmap (:["","","",""]) (surrLazy "(" ")")

parseContent :: [[String]] -> ReadP [String]
parseContent dhaList = do
  let [dhas,aups,ms] = L.transpose dhaList
  headWord <- devanagariOrParen
  stemHint <- csd (surrLazy "[" "]") <++ pure ""
  genderEtc <- gender <++ pure ""
  rest <- R.look
  derivation1 <- maybeP $ csd (derivationWithDhatuOrNot dhaList)
  let derivationAnywhere = (derivationWithDhatuOrNot dhaList) <++ (R.get >> derivationAnywhere)
  derivation2 <- maybeP derivationAnywhere
  let headWordFixed = stemFix stemHint headWord
  let derivation = fromMaybe ["","","","",""] (derivation1 <|> derivation2)
  return $ [headWordFixed, stemHint, genderEtc] ++ derivation ++ [rest]

dropAtEnd :: Int -> [a] -> [a]
dropAtEnd k = reverse . drop k . reverse


stemHintTypoFix :: String -> String
stemHintTypoFix = go . uncomma where
  go "न्तृ" = "तृ"
  go "" = ""
  go x = if last x `elem` "चछजतदनभमरशषसह" then x++"्" else x
  uncomma = filter isDevanagari

-- न्तृ, ष्टु, अपः(स्)
stemFix :: String -> String -> String
stemFix hint = go (stemHintTypoFix hint) where
  go _ "सखा" = "सखि"
  go _ "वायुसखा" = "वायुसखि"
  go _ "पन्थाः" = "पथिन्"
  go _ "अपन्थाः" = "अपथिन्"
  go _ "मन्थाः" = "मथिन्"
  go _ "नग्नहु" = "नग्नहू"
  go "ऋ" x = init x ++ "ृ"
  go _ "राः" = "रै"
  go _ "द्यौः" = "द्यो"
  go _ "गौः" = "गो"
  go "च्" x = dropAtEnd 2 x ++ "च्"
  go "छ्" x = dropAtEnd 2 x ++ "छ्"
  go "ज्" x = dropAtEnd 2 x ++ "ज्"
  go "ञ्च्" x = dropAtEnd 2 x ++ "ञ्च्"
  go _ "अवाङ्" = "अवाञ्च्"
  go _ "अनड्वान्" = "अनडुह्"
  go _ "क्रोष्टा" = "क्रोष्टु"
  go "ष्टृ" x = init x ++ "ृ"
  go "तृ" x = init x ++ "ृ"
  go "त्" x = if any (`L.isSuffixOf` x) ["मान्","वान्"] then dropAtEnd 3 x ++ "त्" else init x ++ "त्"
  go "द्" x = dropAtEnd 2 x ++ "द्"
  go "ध्" x = dropAtEnd 2 x ++ "ध्"
  go _ "सुनु" = "सुनौ"
  go "न्" x = case last x of
               'ा' -> init x ++ "न्"
               'ी' -> init x ++ "िन्"
               _   -> x ++ "न्"
  go "भ्" x = dropAtEnd 2 x ++ "भ्"
  go "म्" "प्रशान्" = "प्रशाम्"
  go "म्" x = init x ++ "म्"
  go "रै" "सुराः" = "सुरै"
  go "रै" "सुरि" = "सुरै"
  go "र्" x = init x ++ "र्"
  go "श्" x = dropAtEnd 2 x ++ "श्"
  go "क्ष्" "काष्ठतट्" = "काष्ठतक्ष्"
  go "ष्" x = dropAtEnd 2 x ++ "ष्"
  go _ "अन्येद्यु" = "अन्येद्युस्"
  go _ "अधरेद्यु" = "अधरेद्युस्"
  go _ "पर्णध्वत्" = "पर्णध्वस्"
  go _ "अङ्गिरा" = "अङ्गिरस्"
  go "स्" x
            | "ाः" `L.isSuffixOf` x = dropAtEnd 2 x ++ "स्"
            | "ान्" `L.isSuffixOf` x = dropAtEnd 2 x ++ "स्"
            | "ः" `L.isSuffixOf` x = dropAtEnd 1 x ++ "स्"
  go _ "पृतनाषाट्" = "पृतनासाह्"
  go "ह्" "श्वेतवाः" = "श्वेतवह्"
  go "ह्" x = dropAtEnd 2 x ++ "ह्"
  go "" x = if last x `elem` "ंः" then init x else x
  go h x = tracePrintu ("WUT", h, x) undefined

deSoftHyphen :: String -> String
deSoftHyphen = go where
  go (c1:'-':c2:cs) = if all isDevanagari [c1,c2] then c1: c2: go cs else c1: '-': c2: go cs
  go (c:cs) = c: go cs
  go [] = []

removeDvitva :: String -> String
removeDvitva = go where
  go ('्':cs@(a:'्':b:rest)) = '्': go (if a==b then drop 2 cs else cs)
  go (c:cs) = c: go cs
  go [] = []

main :: IO ()
main = do
  dhaListAll <-  fmap (LS.splitOn "#") . lines <$> readFile dhatuPath 
  let dhaList = filter ((`notElem` prefixDhatuOverlap) . head) dhaListAll
  skd <- (fmap (removeDvitva . deSoftHyphen . head) . M.elems) <$> load skdPath (M.empty :: M.Map String [String])
  let parsed = (fst . head . parse (parseContent dhaList)) <$> skd
  store skdOut parsed
  let tsv = fmap (\[h,_e,g,i,u,d,p,o,r] -> L.intercalate "\t" [h,g,i,u,d,p,o,r]) parsed
  writeFile tsvOut (unlines tsv)
