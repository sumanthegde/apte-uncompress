{-# LANGUAGE DeriveGeneric #-}
module ApteParser where

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
import ParserCombUtils
import Utils
import Data.Aeson
import qualified GHC.Generics as G
import JsonUtils
import qualified Data.Map as M
import GHC.Base ((<|>), Alternative)
import Data.Functor
import Data.Maybe
import Data.Either
import Control.Lens
import Data.Function
import Control.Applicative (liftA, liftA2)
import Data.Char
import ApteRecords
import ApteUtils
import qualified Data.List.Split as BS8
import Control.Arrow (Arrow(second))

type BS8B = BS8.ByteString


-- | Lens notations primer:
--   <Accessible>: <object> ^. <field>
--
--   Let's call the "dot chain" as 'address'. (Note: it's left-to-right, unlike composition)
--   Recall that (&) === flip ($).
--   Access field: <object> ^. <address>
--   Update field: <object> & <address> .~ <value>
--   Update field: <object> & <address> %~ <function>
--   Map field:    <object> & <address> . to <function>
--   Morph+view: to (\t -> <address> . to <function>) 

--updateRecordMBanner :: String -> String -> RecordM -> RecordM
--updateRecordMBanner key newBanner recordM =
--  recordM & ((at key . _Just . term . _Just . _Right . banner) ?~ Right newBanner)
--
--viewRecordMBanner :: String -> RecordM -> String
--viewRecordMBanner key recordM =
--  recordM & view (at key . _Just . term . _Just . _Right . banner . _Just . _Right)
--
--filteredTerms :: RecordM -> [Term]
--filteredTerms rec1 =
--  rec1 ^.. traverse . term . _Just . _Right . filtered (\t -> isJust (t ^. banner) && isJust (t ^. samasas))

nonchevs = munch (`notElem` "<>")


(///) ideal cope = (Just . Right <$>ideal) <++ (Just . Left <$>cope)

ryt p = Just <$> p
lft p = Just <$> p
oneWhiteSpace = satisfy (`elem` " \n")
s_ p = pJoin [manyGreedy oneWhiteSpace, p]
s1_ p = pJoin [manyGreedy1 oneWhiteSpace, p]
s1'_ p = pJoin [p, manyGreedy1 oneWhiteSpace]

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

fromTo open close = (open ++) <$> surrLazy open close <&> (++ close)
abbr = fromTo "{%<ab>" "</ab>%}"
abbrhyp = fromTo "{%--<ab>" "</ab>%}"
inBraceHash = fromTo "{#" "#}"
inBraceHashDash = fromTo "{#--" "#}"
inParenBraceHashDash = fromTo "({#--" "#})"
inBraceHashParenDash = lookAhead "{#(--" >> bracketed & guard' ("#}" `L.isSuffixOf`)
inParenAbbrHyp = fromTo "({%--<ab>" "</ab>%})"
inBraceHashDeg = fromTo "{#°" "#}"

inSqr = fromTo "[" "]"

nonroots = ("{%<ab>" ++) <$> ["m.","f.","n.","a.","ind."] <&> (++ "</ab>%}")
padiStrings = ("<ab>"++) <$> ["P","A","U"] <&> (++ ".</ab>") --
--padiStrings = ["<ab>P.</ab>", "<ab>A.</ab>", "<ab>U.</ab>"]
rootClasses = manyGreedy $ skipSpaces >> postSkip (pJoin [lit "{c", pInt, lit "c}"]) (lit "," <++ s1_ (lit "or"))
rootPadi = flip postSkip (lit ",") $ skipSpaces >> foldl1 (<++) (fmap lit padiStrings) -- lit "<ab>P.</ab>" <++ lit "<ab>A.</ab>" <++ lit "<ab>U.</ab>"
rootClassesPadi = liftA2 (++) rootClasses (sequence [rootPadi])
romanNumbering = pJoin [lit "{v", munch (`elem` "IV."), lit "v}"]

rootData :: R.ReadP [String]
rootData = filter (not.null) <$> chains (pure (++))
    [ skipSpaces >> (:[]) <$> romanNumbering <++ pure [] -- 15402
    , (concat <$> manyGreedy1 rootClassesPadi) <++ fmap (:[]) rootPadi -- 12952
    , skipSpaces >> (:[]) <$> pJoin [lit "(", regular, lit ")"] <++ pure [] -- 22372
    ]
    <++ -- corner cases 15208, 16056, 26780, 5669, 8199. Only commonality is the mandatory {v..}
    (skipSpaces >> (:[]) <$> pJoin [lit "{v", munch (`elem` "IV."), lit "v}"])

abBrSqr :: ReadP String
abBrSqr = abbr <++ inBraceHash <++ inSqr

abBrSqrs1 :: ReadP [String]
abBrSqrs1 = do
  skipSpaces
  x <- postSkip abBrSqr (lit "," <++ lit "." <++ (skipSpaces >> lit "or"))
  rest <- abBrSqrs1 <++ return []
  return $ x:rest

intraParenDelimiter = "|"
inParen :: ReadP [Char]
inParen = do
  lit "("
  xs <- abBrSqrs1
  lit ")"
  return $ '(': L.intercalate intraParenDelimiter xs ++ ")"

-- | Similar to abBrSqrs, still separated, to avoid nesting of parens.
abBrSqrParens1 :: ReadP [String]
abBrSqrParens1 = do
  skipSpaces
  x <- postSkip (abBrSqr <++ inParen) (lit "," <++ lit ".")
  rest <- abBrSqrParens1 <++ return []
  return $ x:rest

abBrSqrParens1' :: ReadP [String]
abBrSqrParens1' = manyGreedy1 $ postSkip (skipSpaces >> (abbr <++ inBraceHash <++ inSqr <++ inParen)) (lit "," <++ lit ".")

optionally parser = ryt parser <++ return Nothing

manyGreedy1sat = manyGreedy1 . R.satisfy
catManyGreedy1 = fmap concat . manyGreedy1
bracketFree = (`notElem` "[](){}")
bracketed = pJoin [lit "[", regular, lit "]"]
          R.<++ pJoin [lit "(", regular, lit ")"]
          R.<++ pJoin [lit "{", regular, lit "}"]
-- | Parses the longest string in which brackets, if any, are balanced
regular = catManyGreedy1 (manyGreedy1sat bracketFree R.<++ bracketed)
-- | Like regular, but accepts an additional constraint on all maximal bracketed parts
--   Helpful in ending greedy just before an unwanted bracket sequence, like, "{#--..#}".
--   "ButOuter" indicates that the constraint applies on a maximal bracketed part only.
--   Thus, a constraint like `L.isPrefixOf "{#--"` works on "abc {#--xyz#}" but not on "abc [{#--xyz#}]"
regularButOuter condP = catManyGreedy1 (manyGreedy1 (R.satisfy bracketFree) R.<++ guard' condP bracketed)

-- | Like regularButOuter, but the constraint is not applicable to the maximal bracketed part if it
--   starts with {#° and is preceded by "so " or "see " etc. This odd-looking function is to handle the kinds of:
--   - "so {#°...". Example: kawu --graMTi "so {#°BaMgaH#} {#°BadraH}"
--   - "see {#°...". Example: go --tra "see {#°sKalita#} below"
--   - "= {#°...". Example: patraM --AvalI {1}... {2} = {#°Avali#}
--   - "<ab>cf.</ab> {#°". Example: kara --pallava cf. kisalaya#}.
regularButOuterSoSee condBlack = let
  soSee = ["so","see","=","<ab>cf.</ab>"]
  soSeePref = soSee <&> (++ " {#°")
  litSoSee = s_ $ foldr1 (<++) (s1'_.lit <$> soSee)
  jamMidSpaces xs = takeWhile isSpace xs ++ (unwords . words . unwords . lines) (dropWhile isSpace xs)
  optLit s = lit s <++ pure ""
  in catManyGreedy1 $
        manyGreedy1 (aheadSatisfy (\s -> not $ any (`L.isPrefixOf` jamMidSpaces s) soSeePref) >> R.satisfy bracketFree)
        R.<++ pJoin [litSoSee, catManyGreedy1 (s_ (optLit "or " >> lookAhead "{#°" >> bracketed))]
        R.<++ pJoin [litSoSee <++ pure "", guard' condBlack bracketed]

computelNum :: Envt -> Int -> Int
computelNum envt linesLeft = (mapLnu envt M.! loc (head $ locations envt)) + 1 + nLines envt - linesLeft

-- | Problems with the definition of morphism
--  First, there are several candidate definitions
--  1: One that has distinct meaning(s)
--  2: Different spelling, qualifies to be a separate entry (albeit pointing to another entry)
--  3: Any slp1 string in Apte's dictionary. (so that, searching for it always yields some meaningful context)
--  4. Only those starting with -- (or ° in case of Comp)
--
--  3 is simple. But: here, slp1 words in gram (e.g. उणादि) also qualify. They could interfere with actual entry words
--  2 is hard to parse
--  1 misses out those that are written _after_ the meaning, as alternative spelling/word for the preceding morphism (& meaning)
--  4 may be hard too, in that post-Comp phrases may have ° used in different situations
--
--  How about a combination of 4 & 3?
--  4 misses out some °'s in the pre-Comp part.
--  Assuming that they're the only missing ones, we can treat them 2nd class entries & apply 3.
--  Note that उणादि's *usually* don't have ° (Counterex: L<105>) so we don't have false positives
parseMorphism' :: Envt -> ReadP Term
parseMorphism' envt = do
  let a = locations envt
      sp_ = (skipSpaces >>)
      setBanner t term = term {_banner = t}
      setAncestry term = term {_ancestry = ((a++).(:[]).M_) <$> (_banner term <|> head <$> _gram term)}
      setMeanings ms term = term {_meanings = ms}
      setMorphisms vs term = term {_morphisms = vs}
      setComps c term  = term {_samasas = c}
      setGram g term = term {_gram = g}
      (<$.>) f p = (fmap f. fmap Just . sp_) p
      (<$/>) f p = (fmap f. fmap Just . sp_) p <++ return id
      -- rytsp
  skipSpaces
  linesLeft <- (length . filter (=='\n')) <$> look
  banner' <- setBanner <$/> inBraceHashDash
  skipSpaces
  gram' <- setGram <$/> (liftM2 (++) (manyGreedy1 (skipSpaces >>abbrhyp)) (abBrSqrParens1 <++ pure []) <++ abBrSqrParens1 <++ rootData) -- 14993
  let bg = setAncestry $ gram' $ banner' $ termNil {__line = Just (computelNum envt linesLeft)}
  let envt' = envt {locations = fromJust (_ancestry bg)}
  guard $ isJust (_banner bg) || isJust (_gram bg) -- bg's head's prefix is pre-controlled by "morphism-starters" etc
  meanings' <- setMeanings <$.> parseMeanings envt'
  return $ meanings' bg

parseMorphisms' :: Envt -> ReadP [Term]
parseMorphisms' a = manyGreedy1 (parseMorphism' a)

parseMorphisms :: ReadP [Char]
parseMorphisms = skipSpaces >> superstringOfNoneOf' ["{@--Comp.@}"]

parseParenBraceWithinSamasa' :: Term -> ReadP Term
parseParenBraceWithinSamasa' t = do
  let sp_ = (skipSpaces >>)
      setBanner t term = term {_banner = t}
      setGram g term = term {_gram = g}
      (<$/>) f p = (fmap f. fmap Just . sp_) p <++ return id
      (++/) p q = pJoin [p, q <++ pure ""]
      (++.) p q = pJoin [p,q]
  skipSpaces
  lit "("
  banner' <- setBanner <$/> fmap concat (liftA2 (:) (fmap ('(':) inBraceHashDash ++/ (lit "," <++ s1_ (lit "or"))) (manyGreedy $ s_ inBraceHash))
  let myterm = banner' t
  skipSpaces
  gram' <- setGram <$/> (chainMaximal1 [abbrhyp, regular] <++ fmap (:[]) regular)
  let bg = gram' myterm
  guard $ isJust (_banner bg) || (case _gram bg of (Just  (('{':'%':'-':'-':_):_))-> True; _-> False)
  lit ")" -- The '(' was prepended to banner (for ease of distinction from other forms) but this is discarded. Odd but works.
  return bg

-- | Covers examples (nonexhaustive):
-- nIla --patraH … {#(--traM) --padmaM#}
-- nir --Iha … {#(-hA), nirIhatA --tvaM#}
-- punar --ukta … {#(--ktaM), punaruktatA#}
-- yaTA --saMKyam … {#(--KyaM), --saMKyena#}
parseBraceParenWithinSamasa' :: Term -> ReadP Term
parseBraceParenWithinSamasa' t = do
  let sp_ = (skipSpaces >>)
      setBanner t term = term {_banner = t}
      setGram g term = term {_gram = g}
      (<$/>) f p = (fmap f. fmap Just . sp_) p <++ return id
      (++/) p q = pJoin [p, q <++ pure ""]
      (<$.>) f p = (fmap f. fmap Just . sp_) p
  banner' <- setBanner <$.> inBraceHashParenDash
  gram' <- setGram <$/> fmap (:[]) abbr
  return $ gram' $ banner' $ t

parseMorphismWithinSamasa' :: Envt -> ReadP Term
parseMorphismWithinSamasa' envt = do
  linesLeft <- (length . filter (=='\n')) <$> look
  let a = locations envt
      sp_ = (skipSpaces >>)
      setMorphisms vs term = term {_morphisms = vs}
      setComps c term  = term {_samasas = c}
      (<$.>) f p = (fmap f. fmap Just . sp_) p
      myterm = termNil {_ancestry = Just a, __line = Just (computelNum envt linesLeft)}
      setMeanings ms term = term {_meanings = ms}
  skipSpaces
  -- b can be empty. todo: See if b,g should not be separated
  bg <- parseParenBraceWithinSamasa' myterm <++ parseBraceParenWithinSamasa' myterm
  let ancestryValue = a++ [S_M_ $ fromJust (_banner bg <|> head <$> _gram bg)]
  let envt' = envt {locations = ancestryValue}
  meanings' <- setMeanings <$.> parseMeaningsWithinSamasa envt'
  samasas' <- setComps <$.> parseSubSamasas envt' <++ return id -- kzaRa daH (dA) ॰karaH. Todo: re-tree `go tra (trA) ॰kArin` in phase2
  return $ meanings' $ samasas' $ bg {_ancestry = Just ancestryValue}

parseMorphismsWithinSamasa' :: Envt -> ReadP [Term]
parseMorphismsWithinSamasa' a = manyGreedy1 (parseMorphismWithinSamasa' a)

parseMorphismsWithinSubsamasa' :: Envt -> ReadP [Term]
parseMorphismsWithinSubsamasa' envt = do
  linesLeft <- (length . filter (=='\n')) <$> look
  let a = locations envt
      sp_ = (skipSpaces >>)
      setMorphisms vs term = term {_morphisms = vs}
      setComps c term  = term {_samasas = c}
      (<$.>) f p = (fmap f. fmap Just . sp_) p
      myterm = termNil {_ancestry = Just a, __line = Just (computelNum envt linesLeft)}
      setMeanings ms term = term {_meanings = ms}
  skipSpaces
  -- b can be empty. todo: See if b,g should not be separated
  bg <- parseParenBraceWithinSamasa' myterm -- <++ parseBraceParenWithinSamasa' myterm
  let ancestryValue = a++ [(case last a of S_S_ _ -> S_S_M_; S_M_S_ _->S_M_S_M_ ) $ fromJust (_banner bg <|> head <$> _gram bg)]
  guard $ ancestryValue `elem` subsamasaMorphismLocs envt
  _ <- tracePrintu (show ancestryValue) (pure [])
  let envt' = envt {locations = ancestryValue}
  meanings' <- setMeanings <$.> parseMeaningsWithinSamasa envt'
  return $ (:[]) $ meanings' $ bg {_ancestry = Just ancestryValue}

-- | Todo: Check:
--  - Missing a meaning phrase?
--  - Creating a spurious meaning phrase?
--  - Missing a morphism phrase?
--    - Sort of. "; {#°" is parsed as Left in parseMorphisms'.
--  - Creating a spurious meaning phrase?
parseMeanings :: Envt -> ReadP [String]
parseMeanings _ = let
  meaningStarters = ["{@--", "{v"]
  morphismStarters = ["{#--", "{%--"]
  obsoletes = [ "; {#°"] -- Degree handling is delegated to next pass of parsing altogether
  postNumeric = skipSpaces >> regularButOuter (prefixOfNoneOf (meaningStarters ++ morphismStarters))
  numeric = chains (pure (++)) [lit "{@--", pInt, lit "@}"]
  numbered = skipSpaces >> pJoin [numeric, s1_ postNumeric]
  nilBeforeVI = lookAhead "{v" >> pure [] -- 14883 titfpsati
  in chainMaximal1 (postNumeric: repeat numbered) <++ manyGreedy1 numbered <++ const (pure []) nilBeforeVI -- 31248

parseMeaningsWithinSamasa :: Envt -> ReadP [String]
parseMeaningsWithinSamasa envt = let
  (/++) p q = pJoin [p <++ pure "", q]
  (++/) p q = pJoin [p, q <++ pure ""]
  meaningStarters = ('{':).show <$> [1..9] -- ["{1}"]
  morphismStarters = ["({#--", "({%--"]
  nextSamasaStarters = ["{#--"] -- 12233?
  allStarters = meaningStarters ++ morphismStarters ++ nextSamasaStarters ++ ["{#(--"] ++ ["{#°"]
  obsoletes = [ "; {#°"] -- Todo: handle it
  parenAlso = pJoin [lit "(", inBraceHashDash ++/ s1_ (lit "also"), lit ")"]
  morphismAtEnd = skipSpaces >> pJoin [parenAlso, lit ".", s_ (pure "")] -- uru gAya ({#--yaH#}) ... {2} ... ({#--yaM#} also).
  degSentenceContd = pJoin [lit "{#°", s1'_ slpStr, superstringOfNoneOf ["#}"], lit "#}", meaningPlusMorphism] -- arDa caMdra {#°draM dA#}, paScAt tApaM kf
  braceHashNonDegAhead = aheadSatisfy (\s -> "{#" `L.isPrefixOf` trim s && not ("{#°" `L.isPrefixOf` trim s)) -- antar lIna {#°nasya#} {#duHKAgneH#}
  degSentence2Contd = pJoin [inBraceHashDeg, braceHashNonDegAhead >> meaningPlusMorphism] -- Todo: Handle: tri daSa {#°aDyakzaH#} {#°ayanaH#}, catur catvAriMSat {#°riMSa#} {#--Sattama#}
  degSlpLsContd = pJoin [s1'_ inBraceHashDeg, lookAhead "<ls>" >> meaningPlusMorphism] -- kaMWa ASleza {#°upagUQa#} <ls>...
  degContdToMorphismAtEnd = degSentenceContd <++ degSentence2Contd <++ degSlpLsContd
  degSlpSentence = concat <$> chainMaximal1 (inBraceHashDeg: repeat (s_ inBraceHash))
  meaningDegLsContd = pJoin [s1'_ degSlpSentence, lookAhead "<ls>" >> meaningPlusMorphism]
  meaningPlusMorphism = regularButOuterSoSee (prefixOfNoneOf allStarters) ++/ (degContdToMorphismAtEnd <++ morphismAtEnd)
  postNumeric = s_ ((parenAlso <++ inParen) /++ meaningPlusMorphism) -- eka cityA … {2} ({#--yaH#}, {#--yanaH#}); eka kara {2} ({#--rA#}); ahan {#--rAtraH#} ({#--traM#} also)
  numeric i = chains (pure (++)) [lit "{", lit (show i), lit "}"]
  subnumeric = (pJoin [lit "{#°", slpStr, lit "#}"] ++/ lit ",") -- aMtar yAmaH …{2} {#॰pAtraM#}, …
  meaningEmptyAsPerEnv = if locations envt `elem` meaningEmpty envt then pure "" else pfail -- adriH jaH ({#--jA#}) {1}… {2} {#--kanyA…#}.
  numAlpha i = s_$ pJoin [numeric i, s_ (subnumeric /++ postNumeric) <++ s_ meaningEmptyAsPerEnv] -- (irrational) asymmetry: subsamasa is subsumed but morphm is extracted by pruning prev's meaning
  nextSamasaAhead = foldr1 (<++) $ lookAhead <$> nextSamasaStarters
  firstInt = fromMaybe 1 (L.lookup (locations envt) (meaningNot1 envt))
  in liftA2 (:) postNumeric (chainMaximal1 (fmap numAlpha [2..]))
  <++ chainMaximal1 (postNumeric: fmap numAlpha [firstInt..])
  <++ chainMaximal1 (fmap numAlpha [1..])
  <++ (skipSpaces >> nextSamasaAhead >> pure []) -- yaTA pUrva ({#--rvaM#}) {#--pUrvakaM#}, satya vAc ({%--<ab>f.</ab>%}) {#--vAkyaM#}

parseSubsamasa :: Envt -> ReadP Term
parseSubsamasa envt = do
  -- <non so> {#°\w*#} [abbr] [,{#°\w*#} [abbr]]* <non ls, non bracehash>
  -- <non so> ensured already
  let ancestors = locations envt
      sp_ = (skipSpaces >>)
      setBanner t term = term {_banner = t}
      setMeanings ms term = term {_meanings = ms}
      setMorphisms vs term = term {_morphisms = vs}
      setComps c term  = term {_samasas = c}
      setGram g term = term {_gram = g}
      (<$.>) f p = (fmap f. fmap Just . sp_) p
      (<$/>) f p = (fmap f. fmap Just . sp_) p <++ return id
      (++/) p q = pJoin [p, q <++ pure ""]
      (.++.) p q = pJoin [p,q]
      intersperse' _ [] = []; intersperse' d (p:ps) = p: fmap (d .++.) ps -- prevents parsing delims & stopping there
      pada1InBlock1 = pJoin [lit "°", slpStr]
      padaInBrackedBlock = s_ $ pJoin [lit "°" <++ lit "--" <++ pure "", slpStr]
      calateChainMaximal1 delimP = fmap concat . chainMaximal1 . intersperse' delimP
      padasInBracedBlock pada1 = calateChainMaximal1 (s1'_ $ lit ",") (pada1 : repeat padaInBrackedBlock)
      bracedBlock pada1 = pJoin [lit "{#", padasInBracedBlock pada1, lit "#}"] --, s_ abbr <++ pure ""]
      bracedBlock1 = bracedBlock pada1InBlock1 -- tri kAla {#°jYa, °darSin#} {%<ab>a.</ab>%}
      bracedBlocksAfter1 = repeat (bracedBlock padaInBrackedBlock) -- prati kUla {#°kArin, --kfta#}, {#--cArin, --vfMtti#}
      postBannerSanity = aheadSatisfy (\xs -> not $ any (`L.isPrefixOf` trim xs) ["<ls>", "{#"]) -- antar lIna {#°nasya#} {#duHKAgneH#} <ls>
  linesLeft <- (length . filter (=='\n')) <$> look
  bannerValue <- calateChainMaximal1 (s1'_ (lit ","<++pure "")) (bracedBlock1 : bracedBlocksAfter1 ) --`tri daSa {#°aDyakzaH#} {#°ayanaH#}`
  let locValue = case last ancestors of (S_ _) -> S_S_; S_M_ _ -> S_M_S_; _ -> undefined
  let ancestryValue = ancestors ++ [locValue bannerValue]
  let envt' = envt {locations = ancestryValue}
  let myterm = termNil {_ancestry = Just ancestryValue, _banner = Just bannerValue, __line = Just (computelNum envt linesLeft)}
  skipSpaces
  meanings' <- setMeanings <$.> (postBannerSanity >> parseMeaningsWithinSamasa envt')
  morphisms' <- setMorphisms <$.> parseMorphismsWithinSubsamasa' envt' <++ return id
  return $ morphisms' $ meanings' $ myterm

parseSubSamasas a = manyGreedy1 (parseSubsamasa a)

parseSamasa :: Envt -> ReadP String -> ReadP Term
parseSamasa envt braceHashOptDash = do
  let sp_ = (skipSpaces >>)
      setBanner t term = term {_banner = t}
      setMeanings ms term = term {_meanings = ms}
      setMorphisms vs term = term {_morphisms = vs}
      setComps c term  = term {_samasas = c}
      setGram g term = term {_gram = g}
      (<$.>) f p = (fmap f. fmap Just . sp_) p
      (<$/>) f p = (fmap f. fmap Just . sp_) p <++ return id
      (++/) p q = pJoin [p, q <++ pure ""]
      -- rytsp
  linesLeft <- (length . filter (=='\n')) <$> look
  bannerValue <- fmap concat (liftA2 (:) (s_ braceHashOptDash ++/ (lit "," <++ s1_ (lit "or"))) (manyGreedy $ s_ inBraceHash)) -- 10168 puruzaH
  let ancestryValue = locations envt ++ [S_ bannerValue]
  let myterm = termNil {_ancestry = Just ancestryValue, _banner = Just bannerValue, __line = Just (computelNum envt linesLeft)}
  let envt' = envt {locations = ancestryValue}
  skipSpaces
  gram' <- setGram <$/> abBrSqrParens1' -- 10389
  meanings' <- setMeanings <$.> parseMeaningsWithinSamasa envt'
  let m'' = setMorphisms <$.> parseMorphismsWithinSamasa' envt' <++ return id
  let s'' = setComps <$.> parseSubSamasas envt' <++ return id
  let opt = (<++ return id)
  -- This is the (only) place where samasas precedes morphisms. tToList() takes this into account
  [subsamasas',morphisms'] <- sequence [s'',m''] -- new scheme where each of m'' can have its own subsamasa
--[morphisms',subsamasas'] <- sequence [m'', opt s''] -- Usual order: m,s
--                           <++ (reverse <$> sequence [s'', opt m'']) -- go --paH {#°vaDUwI#} … ({#--pakaH#})
--                           <++ return [id,id]
  return $ morphisms' $ subsamasas' $ meanings' $ gram' $ myterm

parseSamasas :: Envt -> ReadP [Term]
parseSamasas a = do
  lit "{@--Comp.@}"
  chainMaximal1 $ parseSamasa a (inBraceHashDash <++ inBraceHash) : repeat (parseSamasa a inBraceHashDash) -- nanAndfpati

-- | isFirst: Obsolete. Was meant to account for the "¦"
--   isMorphism: Meant to account for possible absence of any of banner, gram, morphism & samasa.
--     Banner absence: `{%--<ab>m.</ab>%} {@1@}`, `{vII.v} {c2c} <ab>A.</ab>...`
--     Gram absence: `{#--jaM#}`
--     It appears that, banner & gram cannot both be absent simultaneously. Todo: Check this
parseTerm :: Envt -> ReadP Term
parseTerm envt = do
  input <- look
  let sp_ = (skipSpaces >>)
      setMeanings ms term = term {_meanings = ms}
      setMorphisms vs term = term {_morphisms = vs}
      setComps c term  = term {_samasas = case c of (Just []) -> Nothing; _ -> c}
      setGram g term = term {_gram = g}
      setUnfinished u term = case u of (Just u'@(_:_)) -> term {_unspent = _unspent term <|> u, _spent = _spent term <|> Just (take (length input - length u') input)}; _->term
      (<$.>) f p = (fmap f. fmap Just . sp_) p
      (<$?>) f p = (fmap f. fmap Just . sp_) p <++ (fmap setUnfinished . fmap Just . sp_) (munch (const True))
      (<$/>) f p = (fmap f. fmap Just . sp_) p <++ return id
      -- rytsp
  linesLeft <- (length . filter (=='\n')) <$> look
  bannerValue <- postSkip (optional (superstringOfNoneOf' ["{"]) >> inBraceHash) (lit "¦") -- todo: account for: 1. isFirst 2. (pJoin [lit "--", slpStr])
  let ancestryValue = locations envt ++ [B_ bannerValue] --[fromJust $ _banner myterm])
  let myterm = termNil {_ancestry = Just ancestryValue, _banner = Just bannerValue, __line = Just (computelNum envt linesLeft)}
  skipSpaces
  gram' <- setGram <$/> (abBrSqrParens1' <++ fmap concat (manyGreedy1 $ postSkip (skipSpaces >> rootData) (lit "," <++ lit ".")))
  let envt' = envt {locations = ancestryValue}
  meanings' <- setMeanings <$?> parseMeanings envt'
  morphisms' <- setMorphisms <$/> parseMorphisms' envt' -- (setMorphisms <$> (parseMorphisms' /// parseMorphisms )) <++ return id
  samasas' <- setComps <$?> ((skipSpaces >> parseSamasas envt') <++ (eof >> pure []))
  unfinished' <- setUnfinished <$.> munch (const True)
  return $ unfinished' $ samasas' $ morphisms' $ meanings' $ gram' myterm

multiOccur :: ReadP String -> ReadP [String]
multiOccur p = do
  pref <- manyTill get p
  rest <- multiOccur p <++ ((:[]) <$> munch (const True))
  return $ pref : rest

-- Replace all occurrences of pattern' with new. Undefined behavior in case of overlapping or nested matches
replaceAll :: ReadP String -> String -> String -> String
replaceAll pattern' new txt = let
  results = parse (multiOccur pattern' <++ pure [txt]) txt
  mostOccur = L.maximumBy (compare `on` (length.fst)) results
  in L.intercalate new (fst mostOccur)

-- Remove all substrings matched by the parser.
removeAll :: ReadP String -> String -> String
removeAll pattern' = replaceAll pattern' ""

parseBlock :: Envt -> ReadP (String,Record)
parseBlock envt = do
  lit "<L>"
  serial' <- pNumber
  lit "<pc>"
  pc' <- ryt nonchevs
  lit "<k1>"
  k1' <- ryt slpStr
  lit "<k2>"
  k2' <- ryt $ manyGreedy (R.satisfy (/='$'))
  lit "$"
  term' <- ryt $ parseTerm (envt {locations = [L_ serial']})
  return (serial',Record {_k1=k1', _k2=k2', _term= term'})

makeLBlocks :: Envt -> [String] -> IO RecordM
makeLBlocks envt xs = fmap M.fromList $ forM xs $ \l -> do
  let envt' = envt {nLines = length $ filter (=='\n') l}
  let desc = parse (parseBlock envt') l
  when (null desc) $ do
    putStrLn l
    putStrLn "ERROR IN PARSING ^^^^"
    putStrLn "Enter key to continue"
    getChar
    return ()
  return (fst $ head desc)

positiveEgMap =
  [(1003,"Don't consider {#°samADiyogaH#} a morphism")
  ,(9518,"eka. Meaning should not stop at {@--6@} (ref: {#°pArTiva#}), LATER: --anta, (-taH), viDvaMsin")
  ,(10007,"kanchukin. {%--<ab>m.</ab>%} {@--1@} is a morphism")
  ,(10010,"{#--jaM#} is a morphism. LATER: Comp. {#--jaMH#} is a typo")
  ,(10593,"kas (verb with multi ganas) {vI.v} {c1c} <ab>P.</ab> ({#kasati, kasita#}) To move, go, approach. {vII.v} {c2c}")
  ,(11839,"#kzam# (verb with P in 1 gana, A in another) {c1c} <ab>A.</ab>, {c4c} <ab>P.</ab> ({#kzamate,.... or {#kzamita#})")
  ]
positiveEg = map fst positiveEgMap
counterEgMap =
  [(1956,"anuzaMj (verb: starts with parens) ({#°saMj#}). Also note, later, this: {%--<ab>pass.</ab>%} ( {#--zajyate#}) ")
  ,(1977,"anuzRa. The '({#-- #} <text>)' pattern: {@--Comp.@} {#--guH#} ({#--go#} ray)")
  ,(2044,"anUpa. ({#--pAH#} <ab>pl.</ab>) -> it's plural, not a new word")
  ,(12225,"gagana. The '(<text> {#-- #})' pattern: (Some suppose {#gagaRa#} ⋯ writer: {#--PAlgune gagane Pene#})")
  ]
eg = [(12784)]
counterEg = map fst counterEgMap -- [1956,1977,2044,12225]
isFromSelected = const (const True) $ or . sequence ((\x -> (BS8.pack ("<L>" ++ show x ++ "<") `BS8.isPrefixOf`)) <$> (const [886] (positiveEg ++ counterEg)))
altLf = "`"

abbrHypInSamasaEg = map fst abbrHypInSamasaMap
abbrHypInSamasaMap =
  [(10822,"kAla")
  ,(11375,"kfta")
  ,(12713,"go")
  ,(13322,"cit")
  ,(141,"akzi")
  ,(15561,"dUra")
  ,(17765,"para")
  ,(18538,"paScima")
  ,(21860,"brahman")
  ,(22674,"maMda")
  ,(22944,"mAtrA")
  ,(23456,"meGaH")
  ,(271,"aMgaM")
  ,(27861,"SArdUlaH")
  ,(28519,"zaz")
  ,(29062,"sattvaM")
  ,(29065,"satya")
  ,(886,"adriH")
  ]

th k = take 1 . drop k  

makeInline :: (String -> String) -> [FilePath] -> FilePath -> FilePath -> IO ([String], M.Map Int String)
makeInline lineCurator patchPaths inPath outPath = do
  let lend = BS8.pack "<LEND>"
  ls <- BS8.lines <$> BS8.readFile inPath
  let pageMarks = M.map BS8.unpack $ M.fromList $ filter ((BS8.pack "[Page" `BS8.isPrefixOf`) . snd) (zip [1..] ls)
--  print patchPath
  patches <- traverse (`load` M.empty) patchPaths :: IO [M.Map Integer String]
  let patchFinal = M.unionsWith (\a b -> tracePrintu (a,b) b) patches
--  print linePatch
  lsPatched <- forM ([1..] `zip` ls) $ \(i,l) -> do
    case patchFinal M.!? i of
      Just l' -> putStr (show i ++ " ") >> return (BSU8.fromString l')
      _ -> return l
  let lsPatched' = (BS8.dropWhileEnd (=='\t')) <$> lsPatched -- 31220 "<LEND>\t"
  let lblocks = filter (not.null) $ dropWhile (not. (BS8.pack "<L>" `BS8.isPrefixOf`)) <$> LS.splitOn [lend] lsPatched'
  outH <- openFile outPath WriteMode
  inlines <- forM (filter (isFromSelected . head) lblocks) $ \b -> do
    when (null $ tail b) $ do
      putStrLn "Tail null!"
      print b
      putStrLn "Enter key to continue"
      void getChar
    let inlined = BSU8.toString $ head b <> BS8.pack "$" <> BS8.intercalate (BS8.pack "\n") (tail b)
    let curated = lineCurator inlined
    hPutStrLn outH curated
    return curated
  hFlush outH
  hClose outH
  return (inlines,pageMarks)


--patch1Path = apteOutput </> "1.patch.json"
--rec1Path = apteOutput </> "1.json"
store' path structure = putStrLn ("store: "++path) >> store path structure >> return structure

patchOverlapCheck :: IO ()
patchOverlapCheck = do
  patchesAll <- fmap (ap90</>) . filter ("patch." `L.isPrefixOf`) <$> listDirectory ap90
  patches <- traverse (`load` M.empty) patchesAll :: IO [M.Map Integer String]
  let lineNumsGrouped = (L.group . L.sort. concat) (M.keys <$> patches)
  let dups = filter (not.null.tail) lineNumsGrouped
  if null dups then putStrLn "No overlaps." else putStrLn $ "OVERLAPS: " ++ L.intercalate ", " (show.head <$> dups)

-- | To be run in ghci
type EditPath = FilePath
makeNewPatch :: EditPath -> FilePath -> IO (M.Map Integer String)
makeNewPatch editPath outPath = do
  oldLines <- BS8.lines <$> BS8.readFile originalPath
  patchesAll <- fmap (ap90</>) . filter ("patch." `L.isPrefixOf`) <$> listDirectory ap90
  patches <- traverse (`load` M.empty) patchesAll :: IO [M.Map Integer String]
  let patch = M.unionsWith (\a b -> b) patches
  patchedLines <- forM ([1..] `zip` oldLines) $ \(i,l) -> do
    case patch M.!? i of
      Just l' -> return (BSU8.fromString l')
      _       -> return l
  editedLines <- BS8.lines <$> BS8.readFile editPath
  let patchf i p e = if p == e then Nothing else Just (i, BSU8.toString e)
  let newPatch = M.fromList $ catMaybes $ zipWith3 patchf [1..] patchedLines editedLines :: M.Map Integer String
  patchInOut <- if outPath `elem` patchesAll then load outPath (M.empty :: M.Map Integer String) else pure M.empty
  store outPath (M.union newPatch patchInOut)
  return newPatch

resetAndApplyPatches :: FilePath -> IO ()
resetAndApplyPatches outPath = do
  oldLines <- BS8.lines <$> BS8.readFile originalPath
  patchesAll <- fmap (ap90</>) . filter ("patch." `L.isPrefixOf`) <$> listDirectory ap90
  patches <- traverse (`load` M.empty) patchesAll  :: IO [M.Map Integer String]
  let patch = M.unionsWith (\a b -> b) patches
  patchedLines <- forM ([1..] `zip` oldLines) $ \(i,l) -> do
    case patch M.!? i of
      Just l' -> BS8.putStrLn l >> putStrLn l' >> return (BSU8.fromString l')
      _ -> return l
  BS8.writeFile outPath (BS8.unlines patchedLines)

getMapLnu :: IO (M.Map String Int)
getMapLnu = do
  ls <- lines <$> readFile originalPath
  let getLnum (l,i) = flip (,) i . takeWhile (/='<') <$> L.stripPrefix "<L>" l
  let lis = catMaybes $ getLnum <$> zip ls [1..]
  return $ M.fromList lis

-- ghci helpers
getTermByK k rs = fmap (fromJust . _term) $ M.elems $ M.filter (\r -> _k1 r == Just k) rs
briefAnc t = L.intercalate " / " (map loc $ t^. ancestry . _Just)
hint t = (briefAnc t, takeEnd 1 (LS.splitOn "--" (t^. spent . _Just)), take 100 (t^. unspent . _Just))
anc t = termNil {_banner = pure (briefAnc t), _morphisms = fmap anc <$> (t^. morphisms), _samasas = fmap anc <$> (t^. samasas)}

helper = do
  let store9 fname = store (ap90</>fname)
  rs <- load (ap90</>"0.json") recordMNil
  let ts = rs ^.. traverse . term . _Just
  let us = filter (\t -> isJust (_unspent t)) ts
  let ts1 = ts ^.. traverse . samasas . _Just . traverse
  let _ms1 = filter (\t -> isJust (_morphisms t) && isJust (_samasas t)) ts1
  return ()

main :: IO ()
main = do
  let lineCurator = id
                  . replaceAll (lit "v. {1}") "<ab>v. l.</ab>"
                  . replaceAll (lit ": {#--") ":-- {#"
                  . replaceAll (lit "(\n") ("\n(") -- go paH (\n{#--pikA
               --   . replaceAll (lit ("#}"++altLf++"{#")) " "  -- anti: akula
                  . replaceAll (lit "|") "L" 
                  . replaceAll (lit "{@1@}") "{@--1@}"
                  . replaceAll (surrLazy "<lbinfo" "/>\n") "\n"
                  . replaceAll (surrLazy "[Page" ("]\n")) "\n"
  patchesAll <- fmap (ap90</>) . filter ("patch." `L.isPrefixOf`) <$> listDirectory ap90

  (inlines,pageMarks) <- makeInline lineCurator patchesAll originalPath inlineTxtPath
  _ <- store' pageMarkPath pageMarks
  -- $ (regularButOuter (const True)>>eof)) <++ (Just <$> munch (const True)) ) <$> inlines
--  _ <- store' (ap90</>"_irreg.json") danglingBrackets
  mapLnu' <- getMapLnu
  let zeroEnvt = Envt [] [] [] [] (-1) M.empty
  envt <- load initialEnvt zeroEnvt
  guard $ nLines envt /= -1 || tracePrintu "ERROR: zeroEnvt" False
  let envt' = envt {mapLnu = mapLnu'}
  rec0 <- makeLBlocks envt' inlines
  _ <- store' rec0Path rec0
  putStrLn "Done."

parserMain = main