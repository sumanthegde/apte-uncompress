module ApteExpander where

import Utils
import ApteRecords
import ApteUtils
import Sandhi
import ParserCombUtils
-- import Natva
import ApteParser hiding (main)
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
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment
import System.IO
import Control.Monad.Trans.Except
import Data.Function
import Data.Char
import Data.Bifunctor
import Control.Applicative
import Data.Aeson (object, toJSON, Value)
import Data.Aeson.Key (fromString)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL
import qualified Data.String as DS -- (fromString)
import SqliteUtil

parse' p =  fst . head . parse p
--parseEither p s = case parse p s of [(parsed,"")] -> parsed; _ -> Left ("Parser error: " ++ s)


sylReplaceOnCompatEnd :: String -> String -> Either String String
sylReplaceOnCompatEnd prev m' = if sylCompat prev m' then Right (sylReplace prev m') else Left ("Syl compat error: " ++ prev ++ " " ++ m')

sylReplaceOnCompatEndPlus1 :: String -> String -> Either String String
sylReplaceOnCompatEndPlus1 prev m' = case syllabize m' of
  [s1,s2] -> if sylCompat prev s1 then Right (sylReplace prev s1 ++ s2) else Left ("Syl compat(2) error: " ++ prev ++ " " ++ m')
  _       -> Left ("Syl compat(3) error: " ++ prev ++ " " ++ m')

sylReplaceOnCompatUptoNatvaYantva a b = let
  normal = sylCompat a b
  natva = sylCompat a (case b of 'R':b' -> ('n':b'); _->b)
  yantva_r = sylCompat (case reverse a of 'f':a' -> reverse ('r':a'); _->a) b
  yantva_v = sylCompat (case reverse a of 'u':a' -> reverse ('v':a'); _->a) b
  compat = normal || natva || yantva_r || yantva_v
  in if compat then Right (sylReplace a b) else Left ("Syl compat (4) error: " ++ a ++ " " ++ b)

sylReplaceOnCompatLax :: String -> String -> Either String String
sylReplaceOnCompatLax prev m' = sylReplaceOnCompatEnd prev m'
                              <|> sylReplaceOnCompatEndPlus1 prev m'
                              <|> sylReplaceOnCompatUptoNatvaYantva prev m'

sylMatchScore :: String -> String -> Int
sylMatchScore = go where
  go (c:cs) (d:ds) = if c==d then 1 + (if c `elem` syllableEnds then 0 else go cs ds) else 0
  go _ _ = 0

sylReplaceOn1Align :: String -> String -> Maybe String
sylReplaceOn1Align fullword variation = do
  let sonantlyPrefix = takeWhile (`notElem` syllableEnds) variation
  guard $ not $ null sonantlyPrefix
  let segments = LS.splitOn sonantlyPrefix fullword
  guard $ length segments == 2
  return $ head segments ++ variation

sylReplaceOn1Align' :: String -> String -> Maybe String
sylReplaceOn1Align' fullword variation
  | variation `headIn` syllableEnds = sylReplaceOn1Align fullword (tail variation)
  | otherwise = sylReplaceOn1Align fullword variation

pointwiseJoiner = sylReplaceOnCompatEnd
pointwiseSylAlignable :: [String] -> [String] -> Bool
pointwiseSylAlignable = go where
  go (w:ws) (v:vs) = isRight (pointwiseJoiner w v) && go ws vs
  go [] [] = True
  go _ _ = False

pointwiseSylAlignJoin :: String -> [String] -> [String] -> [Either String String]
pointwiseSylAlignJoin control parentOpts selfOpts = let
  maybeJoins = zipWith (\p a -> if p==control then fromRight Nothing (Just<$>pointwiseJoiner p a) else Nothing) parentOpts selfOpts
  joins = Right <$> catMaybes maybeJoins
  in joins

surjectivishJoin :: String -> [String] -> [String] -> [Either String String]
surjectivishJoin prev parentOpts selfOpts = let
  -- goal: each selfOpt to be pairable with a distinct parentOpt
  sylReplace1Syl p self = case syllabize self of [syl] -> fromRight Nothing (Just <$> sylReplaceOnCompatLax p self); _->Nothing
  sylReplaceOn1AlignOrEnd p self = sylReplaceOn1Align' p self <|> sylReplace1Syl p self
  joins = fmap Right $ catMaybes $ sylReplaceOn1AlignOrEnd prev <$> selfOpts
  selfHasMatch s = if any isJust (sylReplaceOn1AlignOrEnd <$> parentOpts <*> [s]) then [] else [Left $ "surjectivity error: " ++ L.intercalate "/" parentOpts ++ " " ++ s]
  injectivityCheck = if [prev] == take 1 parentOpts then concat $ selfHasMatch <$> selfOpts else []
  in joins ++ injectivityCheck

parserError msg = pure $ [Left $ "Parser error: "++ msg]

hypSlps' :: String -> R.ReadP [String]
hypSlps' alt = lit "{#"
             *> (lit "--" <++ lit alt)
             *> chainMaximal1 (slpStr1 : repeat (manyGreedy1 (satisfy (`elem` ",- ")) *> slpStr1))
             <* lit "#}"

-- To catch {#--parikri-zkri-yA#} etc
hypSlpsHyp :: String -> Bool
hypSlpsHyp = go "" where
  go "" ('-':xs) = go "-" xs
  go "-" (x:xs) = if x `elem` slpCharSet then go "-a" xs else go "" xs
  go "-a" ('-':rest) = True
  go "-a" (x:xs) = if x `elem` slpCharSet then go "-a" xs else go "" xs
  go _ "" = False
  go _ (x:xs) = go "" xs

-- | Remove prathama affix
unvibhakti :: String -> String
unvibhakti cs
  | last cs == 'H' = init cs
  | last cs == 'M' = init cs
  | otherwise = cs

-- Caution: For syllable related work, it's better to leave the trailing anusvara as-is.
anunasikafy :: String -> String
anunasikafy = go where
  anuMap = zip "kKgGNcCjJYwWqQRtTdDnpPbBm" "NNNNNYYYYYRRRRRnnnnnmmmmm"
  go ('M':c:rest) = fromMaybe 'M' (lookup c anuMap): c: go rest
  go "M" = "m"
  go (c:rest) = c: go rest
  go [] = []

anusvarafy :: String -> String
anusvarafy = reverse . go "" where
  go ac "m" = 'M':ac
  go ac (c1:c2:rest) = go ((if c1/=c2 && anunasikafy ['M',c2] == [c1,c2] then 'M' else c1):ac) (c2:rest)
  go ac xs = reverse xs ++ ac

-- | todo(?) Move special cases to Envt config
sandhiE :: String -> String -> String
sandhiE purva = go (unvibhakti purva) where
  go "aMguli" ('s':'a':'M':'g':ys) = go "aMgulizaMg" ys -- 8/3/80
  go "agni"   ('s':'t':'u':'t':ys) = "agnizwut"++ys -- 8/3/82
  go "agni"   ('s':'t':'o':'m':ys) = "agnizwom"++ys -- 8/3/82
  go "jyotis" ('s':'t':'o':'m':ys) = "jyotizwom"++ys -- 8/3/83
  go "Ayus"   ('s':'t':'o':'m':ys) = "Ayuzwom"++ys -- 8/3/83
  go "mAtf"   ('s':'v':'a':'s':ys) = "mAtfzvas"++ys -- 8/3/84
  go "pitf"   ('s':'v':'a':'s':ys) = "pitfzvas"++ys -- 8/3/84
  go "mAtf"   ('p':'i':'t':'f':ys) = "mAtApitf"++ys -- 6/3/25
  go "mAtf"   ('p':'u':'t':'r':ys) = "mAtAputr"++ys
  go "zaz"    ('n':'a':ys)         = "zaRRa"++ys -- 8/4/42 vart.
  -- aluk. todo: add all
  go "Ka"     ('a':'w':'a':ys)     = "Kewa"++ys -- aluk
  go "DI"     ('p':'a':'t':'i':ys) = "DiyAMpati"++ys
  -- rvo
  go "gir"    ('p':ys)             = "gIrp"++ys -- 8/2/76, 8/2/69 vart.
  go "ASis"   ys                   =  go2 "ASIH" ys -- 8/2/76 र्वोरुपधाया
  go "pur"    ys                   =  go2 "pUH" ys
  go "Dur"    ys                   =  go2 "DUH" ys
  go "puMs"   ys@(a:b:_)          =  if a `elem` "kcp" && b `elem` "aAiIuUeEoO" then go2 "puMs" ys else go2 "pum" ys -- 8/3/6 (reduced)
  -- numbers
  go "dvi"    ys                   =  if any (`L.isPrefixOf` ys) ["daS","viMS","triMS"] then "dvA"++ys else go2 "dvi" ys --6/3/47
  go "azwan"  ys                   =  if any (`L.isPrefixOf` ys) ["daS","viMS","triMS"] then "azwA"++ys else go2 "azwan" ys
  go "ahan" ys -- 8/2/69, 8/3/48 approx
    | ys `headIn` "kSzs" = go2 "ahas" ys
    | any (`L.isPrefixOf` ys) ["rAtr","rUp","raT"] = "aho"++ys
    | otherwise = "ahar"++ys
  -- odd
  go "prAC" ys = go2 "prAw" ys
  go xs ys = go2 xs ys
  go2 xs ys = s2e $ sandhiApte (e2s xs) (e2s ys)

strictPrefixOf xs ys = case L.stripPrefix xs ys of Just (_:_) -> True; _ -> False
--sandhiXmw :: M.Map String String -> String -> String -> Either String String
--sandhiXmw mwComps x y = do
--  let deva = (' ':).uncanon . e2s
--  let z = tracePrintu (unwords $ deva<$>[x,y,z']) z' where z' = s2e $ sandhiApte (e2s $ unvibhakti x) (e2s y)
--  zComp <- maybe (Left $ "MW error: not found " ++ z) Right (mwComps M.!? (unvibhakti z))
--  return zComp

data ExpEnv = ExpEnv
  { topK1 :: String
  , parentTerm :: Term
  , thisTerm :: Term
--  , mwDict :: M.Map String String
  , unparenMap :: M.Map String [String]
  , preservingSM :: [[Location]]
  }

-- | Expands feminine forms that are enclosed in 'gram' attribute. (Recall: gram is a list)
--   Usually they're of the form "({#...#}|{%<ab>f.</ab>%})", which we process (if it's a 1-Align, or a ङीप्/ङीष् with 2-Align).
--   Sometimes they may lack paren / have multiple f's / have a m. f. m. f. chain (e.g. चौड (डी) ल (ली).. etc (which we don't process)
--   Important: Call it after look-up based expansion is done (coz the f form might apply to one of them, & it's never the other way.)
--   Todo: Smarter parsing of gram in the parser stage, and using it here (if it benefits)
fFromGramCore :: [String] -> String -> [Either String String]
fFromGramCore expandedBanners fSuffix = let
  cleanSuf = dropWhile (=='-') fSuffix -- obsolete now probably
  getF1Align expandedBanner = sylReplaceOn1Align expandedBanner cleanSuf
  sylReplaceNee expandedBanner = fromRight Nothing (Just <$> sylReplaceOnCompatUptoNatvaYantva expandedBanner cleanSuf)
  getFNee expandedBanner = case reverse <$> syllabize cleanSuf of ['I':_] -> sylReplaceNee expandedBanner; _-> Nothing
  getF b = getF1Align b <|> getFNee b
  allFs = catMaybes $ getF <$> expandedBanners
  in if null allFs then [Left $ "fFromGramCore error: " ++ L.intercalate "/" expandedBanners ++ "  " ++ fSuffix] else Right <$> allFs

fFromGram :: ExpEnv -> [String] -> [Either String String]
fFromGram expEnv expandedBanners = let
  gs = thisTerm expEnv ^. gram . _Just
  getFSufExpr g = case LS.splitOn "#}|{%<ab>f.</ab>%})" g of (f:_:_) -> (Just . reverse . takeWhile (/='#') . reverse) f; _->Nothing
  separateSufs = filter (not.null) . LS.splitWhen (`elem` " ,-")
  expandedFs = concatMap (fFromGramCore expandedBanners) $ concat $ separateSufs <$> catMaybes (getFSufExpr <$> gs)
  in expandedFs

appender :: ExpEnv -> String -> Location -> ExceptT String [] String
appender _ prev (L_ _) = pure prev

-- | 1Align <|> Lookup <|> if fem (1Align + if I CompatEnd)
appender expEnv prev (B_ b) = do
  let (slp0: slps) = filter (not.null) $ LS.splitOneOf "{#}-,; " [if isSpace c then ' ' else c | c <- b]
  let maybeSlp0 = if any ('(' `elem`) (slp0:slps) then Nothing else Just slp0
  let [alignFailMsg,lookupFailMsg] = Left . (++prev++" "++b) <$> ["align based expansion error: ", "lookup based expansion error: "]
  let alignBasedExpansion = maybe alignFailMsg (Right) $ sequence $ maybeSlp0: fmap (sylReplaceOn1Align slp0) slps :: Either String [String]
  let lookupBasedExpansion = maybe lookupFailMsg (Right . (anusvarafy <$>)) ((unparenMap expEnv M.!? (anunasikafy . topK1) expEnv))
  let alignedUnionLookedup = sequence $ alignBasedExpansion <|> lookupBasedExpansion
  let femininesFromGram = fFromGram expEnv (rights alignedUnionLookedup)
  ExceptT $ uniq $ alignedUnionLookedup ++ femininesFromGram

-- | if n:n CompatEnd <|> if n:1 (1Align <|> CompatEnd) <|> (CompatEnds <|> 1Align)
--   n:n aDvanIna, aDvanya --naH, --nyaH
--   default 1Align? kAyaka --kA
appender expEnv prev (M_ m) = do
  let underDhatu = or $ (==) <$> padiStrings <*> (parentTerm expEnv ^. gram . _Just)
  let isDhatuMorph = not $ or $ (==) <$> nonroots <*> (thisTerm expEnv ^. gram . _Just)
  let sandhi' x y = s2e $ e2s (anunasikafy x) `sandhiApte` e2s y -- sandhiApte coz prAdus+as, not sandhiE coz no unvibhaktify
  let dhatuUpaJoin dha upa = sandhi' upa dha
  let parentBanners = (parentTerm expEnv ^. bannerExp . _Just) ^.. traverse . _Right -- Prone to false positives later
  let parentBanner = parentTerm expEnv ^. banner . _Just
  let pointwiseness selfBanners =   (length parentBanners > 1) -- apakAraka --kArin; or anarThya, anarTaka; ...
                                &&  flip any [",","--"] (`L.isInfixOf` parentBanner) -- ... but not kava(ba)ra
                                &&  (length parentBanners == length selfBanners)
                                &&  (pointwiseSylAlignable parentBanners selfBanners)
  let sylReplaceOnCompatOr1Align p a = sylReplaceOnCompatLax p a <|> maybe (Left $ "1Align error: "++p++" "++a) Right (sylReplaceOn1Align' p a)
  let expand ownBanners
        | underDhatu && isDhatuMorph = fmap (Right . dhatuUpaJoin prev) ownBanners
        | pointwiseness ownBanners = pointwiseSylAlignJoin prev parentBanners ownBanners
        | length parentBanners > 1 = surjectivishJoin prev parentBanners ownBanners
        | otherwise = fmap (sylReplaceOnCompatOr1Align prev) ownBanners
  let ignore = (>> pure [Right prev]) -- e.g {%--<ab>Caus.</ab>%}
  let mParser = fmap expand (hypSlps' "--" <*eof) <++ ignore (abbrhyp<++romanNumbering) <++ parserError ("{#"++prev++"#}" ++ " M_ " ++m)
  ExceptT $ uniq $ parse' mParser m


-- | Main work: join purvapada (B_) with this (S_) using sandhi
--   Step 1: Parsing. Stricter than the samasa banner parser (avoids parens etc)
--   Step 2: Sanity. For every non-first current banner, if it's an aligning monosyllabic then expand it and accept for Sandhi.
--     If it's non-mid-aligning polysyllabic then accept (without expanding) e.g. latA in {#roman#}/ {#--rAjiH, --jI, --latA#}
--     Other cases: discard.
--   Step 3: Sandhi. नलोप etc are handled in custom Sandhi function.
--     णत्व is also applied (if hinted by Apte). todo (?) षत्व 
appender expEnv prev (S_ s) = do
  let com = lit "," <++ lit "" -- Ending with "," possible due to loose parsing in parsing stage.
  let hypSlpsNoHSH a = aheadSatisfy (not . hypSlpsHyp) >> hypSlps' a
  let hypSlpss = fmap concat $ skipSpaces >> chainMaximal1 (hypSlpsNoHSH "--": repeat (com *> skipSpaces *> hypSlpsNoHSH "")) <* com <* eof
  let acceptIfFullReplacement var result = if result == var then Right var else Left $ "S error: midAlign " ++ var ++ " " ++ result
  let acceptIfNoBlending pre this = maybe (Right this) (acceptIfFullReplacement this) (sylReplaceOn1Align pre this)
  let expandAcceptOrDiscard pre this
        | length (syllabize this) == 1 = sylReplaceOnCompatEnd pre this <|> Left ("S error: 1 syl "++pre ++ " " ++ this)
        | otherwise = acceptIfNoBlending pre this
  let markLeftRight (x:xs) = Right x: zipWith expandAcceptOrDiscard (x:xs) xs; markLeftRight xs = Right <$> xs
  let sParser = (markLeftRight <$> hypSlpss) <++ parserError ("{#"++prev++"#} S_ " ++ s)
  let z = (parse' sParser s) :: [Either String String]
  uttara <- ExceptT z
  let natvaHintInGram g = case LS.splitOn "({#" g of [_,w] -> 'R' `elem` w; _-> False; -- hack for rAma+ayana=rAmAyaRa etc
  let natvaHinted = any natvaHintInGram (thisTerm expEnv ^.. gram . _Just . traverse)
  let natvafy xs = uncurry (++) $ second (('R':).tail) (break (=='n') xs)
  let uttaraWithNatva = if ('n' `elem` uttara && natvaNext (e2s prev) && natvaHinted) then natvafy uttara else uttara
  let samasta = sandhiE prev uttaraWithNatva
  return samasta

-- | Simple slp parsing suffices
appender _ prev (S_S_ s) = do
  let nonSlps = munch1 (`notElem` slpCharSet)
  let slpss = nonSlps *> manyGreedy1 (slpStr1 <* nonSlps)
  uttara <- ExceptT $ Right <$> (parse' slpss s)
  let samasta = sandhiE prev uttara
  return samasta

-- | S_M_ values are of two kinds.
--   For the `<ab>f.</ab>` kind, the final word is the upstream value (S_) itself.
--   For the ः,ा,ी kind, the final word is derived via align-replace.
--   However, if the present term is a S_M_S_*, then here we are supposed to
--   pass not the final word but the "forepart" of the prospective comound.
--   The forepart is usually the S_ value itself (e.g. गो-त्र-(त्रा)-कर्तृ),
--   unless S_M_ is in preservingSM (e.g. क्षण-(द)-दा-करः)
appender expEnv prev l@(S_M_ m)
  | any (`L.isPrefixOf` (thisTerm expEnv ^. ancestry . _Just)) (preservingSM expEnv)
    || l == last (thisTerm expEnv ^. ancestry . _Just) = do
         let nonSlps = munch1 (`elem` "({#--° ,}")
         let slpss = nonSlps *> manyGreedy1 (slpStr1 <* nonSlps)
         let gramVariant = abbrhyp *> return [prev]
         let err i = Left $ "S_M_ " ++ show i ++ " error: " ++ prev ++ " " ++ m
         let errorOnEmpty ts = if null ts then [err 0] else Right <$> (fst . head $ ts)
         cur <- ExceptT $ errorOnEmpty (parse (slpss <++ gramVariant) m)
         let ans = maybe (err 1) Right (sylReplaceOn1Align' prev cur) -- todo: try 1Align <|> CompatEnd (e.g. jalaja)
         ExceptT [ans]
  | otherwise = return prev

-- | Same as for S_S_
appender _ prev (S_M_S_ s) = do
  let nonSlps = munch1 (`notElem` slpCharSet)
  let slpss = nonSlps *> manyGreedy1 (slpStr1 <* nonSlps)
  uttara <- ExceptT $ Right <$> (parse' slpss s)
  let samasta = sandhiE prev uttara
  return samasta

appender _ prev cur = ExceptT [Left $ "Default Folder error: " ++ prev ++ " " ++ show cur]

folder :: ExpEnv -> [Either String String] -> Location -> [Either String String]
folder expEnv prevs cur = runExceptT $ do
  prev <- ExceptT prevs
  appender expEnv prev cur

morphismFolder :: ExpEnv -> Term -> [Either String String]
morphismFolder expEnv t = let
  ls = t ^. ancestry . _Just
  ans = L.foldl' (folder expEnv) [Right ""] ls
  in ans

--morphismsFolder :: Int -> M.Map String String -> [(Term,String)] -> IO [Either String String]
--morphismsFolder c mwComps tks = fmap concat $ forM (drop c $ zip [1..] tks) $ \(i,(t,k)) -> do
--  dsal <- getDsalBoth
--  let expEnv = ExpEnv k termNil t dsal
--  let m = morphismFolder expEnv t
--  if any isLeft m then do
----    putStrLn ""
----    putStr (show i)
----    putStrLn $ tail $ dropWhile (/='/') $ briefAnc t -- (t ^. ancestry . _Just)
----    mapM_ putStrLn (lefts m) >> void getChar
--    return m
--  else do
--    pure ()
--  --  mapM_ putStrLn (fmap (".    "++) $ rights m)
--    return m
--
--collectBanners :: Int -> [(Term,String)] -> M.Map String [String] -> IO [Either String String]
--collectBanners c' tks ds = reverse <$> go (makeExpEnv <$> tks) c' (map fst tks) where
--  makeExpEnv (t,k) = ExpEnv k termNil t ds
--  go expEnvs c ts = do
--    let foldM2 ac inp f = foldM f ac inp
--    foldM2 [] (drop c (zip expEnvs ts)) $ \ac (e,t) -> do
--      let m = morphismFolder (e {thisTerm = t}) t
--      if any isLeft m then do
----          putStrLn ""
----          putStr (show i)
----          putStrLn $ tail $ dropWhile (/='/') $ briefAnc t -- (t ^. ancestry . _Just)
----      --  either ((>> (void getChar)) . putStrLn ) (const (pure ()))
----          mapM_ putStrLn (lefts m) >> void getChar
--          return $ reverse m ++ ac
--      else do
--        let
--          self = t {_morphisms = Nothing, _samasas = Nothing, _meanings = Nothing}
--          morphs = maybe [] (concatMap tToList) (_morphisms t)
--          sams = maybe [] (concatMap tToList) (_samasas t)
--          kidsInOrder = case (last (t^. ancestry . _Just)) of S_ _ -> sams ++ morphs; _ -> morphs ++ sams
--        m' <- go (repeat e) 0 kidsInOrder
--        return $ reverse m' ++ reverse m ++ ac

traverseTerm :: ExpEnv -> Term -> IO Term
traverseTerm e t = do
    let expandedBanners = morphismFolder (e {thisTerm = t}) t
    let tb = t {_bannerExp = Just $ uniq expandedBanners}
    if any isLeft expandedBanners then return tb else do
      let e' = e {parentTerm = tb}
      let samsFirst = case (last (t^. ancestry . _Just)) of S_ _ -> True; _->False;
      case (last (t^. ancestry . _Just)) of
        S_ _ -> do -- In case order becomes important in future..
          tss <- case _samasas t of Nothing -> pure Nothing; Just ss -> Just <$> forM ss (traverseTerm e')
          tmm <- case _morphisms t of Nothing -> pure Nothing; Just mm -> Just <$> forM mm (traverseTerm e')
          return $ tb {_samasas = tss, _morphisms = tmm}
        _ -> do
          tmm <- case _morphisms t of Nothing -> pure Nothing; Just mm -> Just <$> forM mm (traverseTerm e')
          tss <- case _samasas t of Nothing -> pure Nothing; Just ss -> Just <$> forM ss (traverseTerm e')
          return $ tb {_samasas = tss, _morphisms = tmm}

getDsal = do
  let rephaFix xs = map (\c -> if c == 'ऱ' then 'र' else c) xs
  let anunasikafyS = uncanon . e2s . anunasikafy . s2e . canon
  let nFix = filter (/= 'ऩ')
  let badMap = fmap $ \x -> fromMaybe x (lookup x (zip "ᳲ:ढ़" "ःःढ"))
  dsal <- fmap (fmap (anunasikafyS . badMap . nFix . rephaFix)) <$> load (apteDir  </> "dsal/dsal.json") ([]::[[String]])
  --let validChar c = c `elem` ('्': Aaz.sans ++ catMaybes (fmap (`lookup` sigMap) (tail vowelsReal)))
  --forM (zip [1..] dsal) (\(i,css) -> if any (not.validChar) (concat css) then printu (filter (not.validChar) (concat css)) >> void getChar else pure ())
  let dsalE = fmap (s2e . canon) <$> dsal
  return dsalE

getDsal2 fpath = do
  dlines <- lines <$> readFile fpath
  let kvpair l = let [k,vs] = LS.splitOn ("=") l in (k, LS.splitOn (";") vs)
      kvpairs = kvpair <$> dlines
  return $ M.fromList kvpairs

getDsalBoth = do
  dParen <- load (dsalDir </> "dsalE.json") ([] :: [[String]])
  dPatch <- load (dsalDir </> "dsalE_errata.json") ([] :: [[String]])
  let dExpanded = M.elems $ M.fromList $ (fmap head dParen `zip` dParen) ++ (fmap head dPatch `zip` dPatch)
--  d1 <- getDsal2 (apteOutput </> "dep1.txt")
  customExpandedTable <- lines <$> readFile (dsalDir </> "customExpanded.txt")
  let customExpanded = LS.splitOn ";" . (!!1) . (LS.splitOn "=")  <$> customExpandedTable
  let expanded = uniq $ dExpanded ++ customExpanded
  let revMap = M.fromList [(d,di) | di <- expanded, d<-di]
  return revMap --  $ M.union d1 d2

--readCompPairs :: FilePath -> IO [(String,String)]
--readCompPairs filePath = do
--  ls <- lines <$> readFile filePath
--  let getK1 "" = Nothing; getK1 l = if "<k1>" `L.isPrefixOf` l then Just (takeWhile (/='<') (drop 4 l)) else getK1 (tail l)
--  let allK1s = catMaybes $ getK1 <$> ls
--  return $ zip allK1s (repeat "")

------ ghci helpers -------
samasaLexOrderCheck :: [String] -> Term -> [(Int, String, String, String)]
samasaLexOrderCheck lexExceptions t = let
  t's = t^. samasas . _Just
  b t' = (e2s' . anunasikafy . trim) (t' ^. banner . _Just)
  l t' = fromJust (t' ^. _line)
  comparer t1 t2 = if b t1 <= b t2 || briefAnc t1 `elem` lexExceptions then Nothing else Just (l t1, briefAnc t, b t1, b t2)
  lexBlips = catMaybes $ zipWith comparer t's (tail t's)
  in lexBlips

run :: Int -> Int -> IO [Term]
run start len = do
--  let mwXmlPath = dictDir </> "mw" </> "txt" </> "mw.txt"
--  mwCompounds <- pure M.empty -- M.fromList <$> readCompPairs mwXmlPath
  d <- getDsalBoth
  rs <- load (apteOutput</>"0.json") recordMNil
  pSM <- load (apteDir </> "preserve_S_M.json") ([]::[[Location]])
  let tks = (\r -> (r ^. term & fromJust, r^. k1 . _Just)) <$> M.elems rs -- rs ^.. traverse . term . _Just
  let tks1 = L.sortOn (\(t,k) -> let (L_ l:_) = t^. ancestry . _Just in (read l:: Float)) tks
  let ts1 = map fst tks1
  let ts2 = concat $ tToList <$> ts1
  let ts3 = filter (\t -> let a= (t^. ancestry . _Just) in (case a of (L_ _: B_ _:[])-> True; _ -> False)) ts2
  let ts4 = filter (\t -> let a= (t^. ancestry . _Just) in (case a of (L_ _: B_ _: M_ _:[])-> True; [_,_] -> True; _ -> False)) ts2
  let ts5 = filter (\t -> let a= (t^. ancestry . _Just) in (case a of (L_ _: B_ _: S_ _:[])-> True; _ -> False)) ts2
  let ts6 = filter (\t -> let a= (t^. ancestry . _Just) in (case a of (L_ _: B_ _: S_ _:S_M_ _:[])-> True; _ -> False)) ts2
--  morphismsFolder k mwCompounds ts3
--  collectBanners k ts1 d
  let expEnv t k = ExpEnv
        { topK1 = k
        , parentTerm = termNil
        , thisTerm = t
        , unparenMap = d
        , preservingSM = pSM
        }
  let traverseTermE (t,k) = traverseTerm (expEnv t k) t
  forM (take len $ drop start tks1) traverseTermE

shardAndStore :: [Term] -> IO ()
shardAndStore es = do
  createDirectoryIfMissing True koshaNestedPath
  forM_ es $ \e -> do
    let numL = loc $ head (e ^. ancestry . _Just)
    store (koshaNestedPath </> (numL ++ ".json")) e

expanderMain :: IO [Term]
expanderMain = run 0 33333

stableUndup :: Eq a => [a] -> [a]
stableUndup = go where
  go (x:y:zs) = if x==y then y: go zs else x: go (y:zs)
  go xs = xs

stripMeaningNum :: String -> String
stripMeaningNum s = fst $ head $ flip parse s $ do
  meaningNum <- surrLazy "{@" "@}" <++ pJoin [lit "{", pInt, lit "}"] <++ pure ""
  munch (const True)

getLocBannerMeanings :: Term -> (String, String, [String])
getLocBannerMeanings t = let
  l = whitespaceFix $ showLocs (t ^. ancestry . _Just)
  ms = t ^. meanings . _Just
  whitespaceFix = unwords . words . unwords . lines
  removeAngularBraces = replaceAll (surrLazy "<" ">") ""
  curateMeaning m = whitespaceFix $ braceHashToDevanagari $ removeAngularBraces $ stripMeaningNum $ whitespaceFix m
  bs = whitespaceFix $ L.intercalate "," $ rights (t ^. bannerExp . _Just)
  in (l, bs, curateMeaning <$> ms)

-- Helper to format metadata for TSV
formatMetadataTSV :: [(Int, (String, String, [String]))] -> [String]
formatMetadataTSV indexedLbms =
 map (\(idx, (locStr, bannerExpStr, _)) -> show idx ++ "\t" ++ locStr ++ "\t" ++ bannerExpStr) indexedLbms

-- Helper to format meanings for TSV
formatMeaningsTSV :: [(Int, (String, String, [String]))] -> [String]
formatMeaningsTSV indexedLbms =
 concatMap (\(idx, (_, _, meaningStrs)) -> map (\m -> show idx ++ "\t" ++ m) meaningStrs) indexedLbms

tabulate :: [Term] -> IO ()
tabulate es = do
  let esFlat = concat $ tToList <$> es
      --showAncestry e = unwords.lines $ showLocs (e ^. ancestry . _Just)
--      toDevanagari = uncanon . e2s'
      stripAndSqueeze = unwords . words . unwords . lines
      eToExps e = (fmap (uncanon . e2s') . rights) (e ^.. bannerExp . _Just . traverse)
      eToAncestor e = ((braceHashToDevanagari) (loc ((e ^. ancestry . _Just)!!1)))
      eToAncestry e = braceHashToDevanagari ((stripAndSqueeze . showLocs) (e ^. ancestry . _Just))
      eToLineNum e =  show (fromJust $ __line $ e)
      eExpToRow e exp = [eToLineNum e, exp, eToAncestry e]
      eToRows e = eExpToRow e <$> (eToExps e) -- [[exp, a] | exp <- eToExps e, let a = eToAncestor e, l=eToLineNum e, as = eToAncestry e]
      table = unlines $ stableUndup $ fmap (L.intercalate " : ") $ concat $ eToRows <$> esFlat
      tablePath = apteOutput </> "table.txt"
      eToL e = (loc ((e ^. ancestry . _Just)!!0))
      eToTag e = last (e ^. ancestry . _Just)
      eToExps_new e = (fmap e2s' . rights) (e ^.. bannerExp . _Just . traverse)
      eExpToRow_new e exp = (exp, eToTag e, eToL e)
      eToRows_new e = eExpToRow_new e <$> eToExps_new e
      calate_132 (exp,tag,l) = exp ++ ":" ++ l ++ ":" ++ (head . words . showLoc) tag
      table_new = unlines $ fmap calate_132 $ stableUndup $ L.sort $ concatMap eToRows_new esFlat
      tablePath_new = apteOutput </> "table_new.txt"
  writeFile tablePath table
  putStrLn $ "Successfully stored the mapping at " ++ tablePath
  writeFile tablePath_new table_new
  putStrLn $ "Successfully stored new mapping at " ++ tablePath_new

sqliteStore :: [Term] -> IO ()
sqliteStore es = do
  let esFlatWithMeanings = concat $ tToListWithF id <$> es
      lbms = getLocBannerMeanings <$> esFlatWithMeanings
      indexedLbms = zip [1..] lbms
      -- metadataLines = formatMetadataTSV indexedLbms
      -- meaningsLines = formatMeaningsTSV indexedLbms
      -- metadataFilePath = apteOutput </> "metadata.tsv"
      -- meaningsFilePath = apteOutput </> "meanings.tsv"
      -- mIndexesSorted = L.sortOn fst mIndexes
      -- mIndexesTable = unlines $ fmap (\(tnum, m) -> tnum ++ "\t" ++ m) mIndexesSorted
      -- mIndexesTablePath = apteOutput </> "mIndexesTable.txt"  
      metadataRows = map (\(idx, (locStr, bannerExpStr, _)) -> (fromIntegral idx, locStr, bannerExpStr)) indexedLbms
      metadataTsvPath = apteOutput </> "metadata.tsv"
      formatMetadataRow (idx, locStr, bannerExpStr) = show idx ++ "\t" ++ locStr ++ "\t" ++ bannerExpStr
      metadataContent = unlines $ map formatMetadataRow metadataRows
      meaningsRows = concatMap (\(idx, (_, _, meaningStrs)) -> map (\m -> (fromIntegral idx, m)) meaningStrs) indexedLbms
      meaningsTsvPath = apteOutput </> "meanings.tsv"
      formatMeaningRow (idx, meaningStr) = show idx ++ "\t" ++ meaningStr
      meaningsContent = unlines $ map formatMeaningRow meaningsRows
  writeFile metadataTsvPath metadataContent
  putStrLn $ "Successfully wrote metadata TSV to " ++ metadataTsvPath
  writeFile meaningsTsvPath meaningsContent
  putStrLn $ "Successfully wrote meanings TSV to " ++ meaningsTsvPath
  -- Clear existing data and bulk load using executeMany
  bulkLoadFromTSV metadataRows meaningsRows
  putStrLn $ "Successfully bulk loaded data into sqlite db using executeMany."

koshaFormContent :: Term -> String
koshaFormContent t = let
  stripAndSqueeze = unwords . words . unwords . lines
  concatBannerGramMeaning t = stripAndSqueeze $ unwords ((t ^. banner . _Just):(t ^. gram . _Just) ++ (t ^. meanings . _Just))
  concatMorphisms t = stripAndSqueeze $ unwords $ concatBannerGramMeaning <$> (t ^. morphisms . _Just)
  in braceHashToDevanagari $ unwords [concatBannerGramMeaning t, concatMorphisms t]

koshaFormJsonReady :: M.Map Int String -> [Term] -> [[(String, String, String, String)]]
koshaFormJsonReady pageMarkMap es = let
  pratipadikafy w = if flip any ["aM","aH","iH","IH","uH","UH"] (`L.isSuffixOf` w) then init w else w
  pratipadikafys e = pratipadikafy <$> rights (e ^. bannerExp . _Just)
  getL e = (loc . head) (e ^. ancestry . _Just)
  getPnum e = maybe ("-") snd $ M.lookupLE (fromJust $ __line e) pageMarkMap
  koshaFormObjectsDirect e =  [((uncanon . e2s) w, getL e, getPnum e, koshaFormContent e) | w <- pratipadikafys e]
  samasas_both_S_and_M_S e = (e ^. samasas . _Just) ++ (e ^. morphisms . _Just . traverse . samasas . _Just)
  koshaFormObjects e = koshaFormObjectsDirect e ++ concat [koshaFormObjects s | s <- samasas_both_S_and_M_S e]
  koshaFormObjectss = L.groupBy ((==) `on` fst4) (L.sortOn fst4 . concat $ koshaFormObjects <$> es)
  in koshaFormObjectss

koshaFormShardAndStore :: [Term] -> IO ()
koshaFormShardAndStore es = do
  pageMarkMap <- M.mapKeys read <$> load pageMarkPath M.empty :: IO (M.Map Int String)
  createDirectoryIfMissing True koshaFlatPath
  forM_ (koshaFormJsonReady pageMarkMap es) $ \wepcs -> do
    let objKeys = ["word", "eid", "pagenum", "content"]
        mapify (w,e,p,c) = object $ zip (fromString <$> objKeys) (toJSON <$> [w,e,p,c])
        obj = mapify <$> wepcs 
        w = fst4 (head wepcs)
        wpath = koshaFlatPath </> (w ++ ".json")
        keyConfig = defConfig { confCompare = keyOrder (DS.fromString <$> objKeys)}
        jsonOutput = encodePretty' keyConfig obj
    BL.writeFile wpath jsonOutput

tabSimple :: [Term] -> FilePath -> IO ()
tabSimple es k1ReverseMapPath = do
  k1ReverseMap <- load k1ReverseMapPath (M.empty :: M.Map String String)
  let esFlat = concat $ tToList <$> es
      --showAncestry e = unwords.lines $ showLocs (e ^. ancestry . _Just)
--      toDevanagari = uncanon . e2s'
      stripAndSqueeze = unwords . words . unwords . lines
      isDevanagari c = ord c >= 0x0900 && ord c <= 0x097F
      eToExps e = (fmap (uncanon . e2s') . rights) (e ^.. bannerExp . _Just . traverse)
      eToAncestor e = ((takeWhile isDevanagari . removeParenthesized . braceHashToDevanagari) (loc ((e ^. ancestry . _Just)!!1)))
      overrideK1 k = maybe k (uncanon.e2s) (k1ReverseMap M.!? (s2e.canon) k)
      eToRows e = [[exp, a] | exp <- eToExps e, let a = (overrideK1 . eToAncestor) e]
      table = concat $ eToRows <$> esFlat
  store (apteOutput</>"simpleTable.json") table