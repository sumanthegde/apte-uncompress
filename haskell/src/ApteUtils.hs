module ApteUtils where

import Utils
import Data.List.Split
import Data.Bifunctor
import Data.Function
import ApteRecords
import Data.Maybe (fromJust, fromMaybe)
import Control.Lens
import Data.Char
import Text.ParserCombinators.ReadP
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad
import SEncode

takeEnd k = reverse . take k . reverse
devanagari="अआइईउऊ"++"ऋॠऌॡ"++"एऐओऔंः"++"ँऽ"++"कखगघङ"++"चछजझञ"++"टठडढण"++ "तथदधनपफबभमयरलव"++"शषसहळ"
slpCharSet= "aAiIuU"++"fFxX"++"eEoOMH" ++"~'"++"kKgGN" ++"cCjJY" ++"wWqQR"++"tTdDnpPbBmyrlv"++"SzshL"
syllableEnds = "aAiIuU"++"fFxX"++"eEoOMH"++"~'"
syllableConts = slpCharSet L.\\ syllableEnds
slpStr = munch (`elem` slpCharSet)
slpStr1 = munch1 (`elem` slpCharSet)
trim = dropWhile isSpace
e2s' = map (\x -> fromMaybe x (e2sMap M.!? x))

syllabize :: String -> [String]
syllabize = let
  oversplit = split (dropFinalBlank $ keepDelimsR $ oneOf syllableEnds)
  yogavahaFix = go [] where
    go :: [String] -> [String] -> [String]
    go ac [] = reverse ac
    go ac (cs:"M":rest) = go ((cs++"M"):ac) rest
    go ac (cs:"H":rest) = go ((cs++"H"):ac) rest
    go ac (cs:rest) = go (cs:ac) rest
  in yogavahaFix . oversplit

-- | Checks if a string is a syllable-suffix of another (or vice versa)
--   i.e. if the syllables of a string match those of a suffix of the other, up to vowels & yogavahas
--   शुल्व - ल्वा ✔
--   पूर्वी - र्व्या ✘
--   Code below can be simplified. (todo)
sylCompat :: String -> String -> Bool
sylCompat = eqEnd `on` (reverse . syllabize . trimNonslp2) where
  trimNonslp = dropWhile (`notElem` slpCharSet)
  trimNonslp2 = reverse . trimNonslp . reverse . trimNonslp 
  onlySonants = dropWhile (`elem` syllableEnds)
  eqEnd sylsr1 sylsr2 = and $ zipWith ((==) `on` (onlySonants . reverse )) sylsr1 sylsr2

sylReplace :: String -> String -> String
sylReplace = replace `on` (reverse . syllabize . trimNonslp2) where
  trimNonslp = dropWhile (`notElem` slpCharSet)
  trimNonslp2 = reverse . trimNonslp . reverse . trimNonslp 
  replace full = concat . go [] full
  go ac (f:fulls) (p:patches) = go (p:ac) fulls patches
  go ac fulls _ = reverse fulls ++ ac
  
tToList :: Term -> [Term]
tToList t = let
  self = t {_morphisms = Nothing, _samasas = Nothing, _meanings = Nothing} 
  morphs = maybe [] (concatMap tToList) (_morphisms t)
  sams = maybe [] (concatMap tToList) (_samasas t)
  kidsInOrder = case last (t^. ancestry . _Just) of S_ _ -> sams ++ morphs; _ -> morphs ++ sams
  in self: kidsInOrder

isMorphismWithinSubsamasa :: (Term, Term) -> Bool
isMorphismWithinSubsamasa (this, next) =
  case (_ancestry this, _ancestry next) of -- ideally, in the pattern match below, should match L_, B_, S_ all.
    (Just [L_ _, B_ _, S_ _, S_S_ ss], Just [L_ _, B_ _, S_ _, S_M_ sm]) -> sylCompat ss sm
    (Just [L_ _, B_ _, S_ _, S_M_ _, S_M_S_ ss], Just [L_ _, B_ _, S_ _, S_M_ sm]) -> sylCompat ss sm
    _ -> False

-- | Only 3 cases found: दुर्-ग(-गः)॰कारक(-कः),  दुर्-ग(-गः)॰व्यसन(-नः),  सु-धी(f)॰उपास्य(-यः)
morphismsWithinSubsamasa :: Term -> [(Term,Term)]
morphismsWithinSubsamasa t = let
  ts = tToList t
  ans = filter isMorphismWithinSubsamasa (ts `zip` tail ts)
  in ans

-- | Helps prepare envt. Breaking, unmaintained.
loadEnvtSubsamasaMorphisms :: Envt -> RecordM -> Envt
loadEnvtSubsamasaMorphisms e rs = let
  ts = rs ^.. traverse . term . _Just
  subsamasaMorphisms = filter (not.null) $ concat $ morphismsWithinSubsamasa <$> ts
  extendLocHasty (t1,t2) = (t1^. ancestry . _Just) ++ takeEnd 1 (t2^. ancestry . _Just)
  fixLabel rev@(end:S_S_ _:rest) = S_S_M_ (loc end): tail rev
  fixLabel rev@(end:S_M_S_ _:rest) = S_M_S_M_ (loc end): tail rev 
  extendLocProper = reverse . fixLabel . reverse . extendLocHasty
  in e {subsamasaMorphismLocs = fmap extendLocProper subsamasaMorphisms}


braceHashToDevanagari :: String -> String
braceHashToDevanagari = go "" where
  go ac ('{':'#':rest) = go' ac "" rest
  go ac (c:rest) = go (ac++[c]) rest
  go ac "" = ac
  go' ac b ('#':'}':rest) = go (ac++ uncanon (reverse b)) rest
  go' ac b (c:rest) = go' ac (e2s' [c]++b) rest

removeParenthesized :: String -> String
removeParenthesized = go "" where
  go ac ('(':rest) = go' ac rest
  go ac (c:rest) = go (ac++[c]) rest
  go ac "" = ac
  go' ac (')':rest) = go ac rest
  go' ac (c:rest) = go' ac rest


rmLine t = t {__line = Nothing}
rmSamasa t = t {_samasas = Nothing}
rmMorphism t = t {_morphisms = Nothing}
rmMeaning t = t {_meanings = Nothing}
rmBanExp t = t {_bannerExp = Nothing}
mvAncBan t = t {_banner = showLocs <$> (t^. ancestry), _ancestry = Nothing}
doRec f t = f $ t {_samasas = fmap (doRec f) <$> (_samasas t), _morphisms = fmap (doRec f) <$> (_morphisms t)}