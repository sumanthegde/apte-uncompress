{-# LANGUAGE OverloadedStrings  #-}
module Pratyayas where
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Utils
import SEncode (canon)

--upasargas = ["अति" ,"अधि","अनु","अप","अपि","अभि","अव","आङ्","उत्","उप","दुस्","नि","निस्","परा","परि","प्र","प्रति","वि","सम्","सु","निर्","दुर्"]
upasargas :: [String] 
upasargas = ["अति" ,"अधि","अनु","अप","अपि","अभि","अव","आङ्","उत्","उप","दुस्","नि","निस्","परा","परि","प्र","प्रति","वि","सम्","सु"]

upasargaWithR = ["परा","परि","प्र","प्रति"] -- | To handle पलायन etc. 8.2.19 उपसर्गस्यायतौ. TODO: Create lookup map for these
upasCanon = canon <$> emptyUpa:upasargas
upasRankList = zip upasCanon [emptyUpaNum..]
upasRankMap = M.fromList upasRankList :: M.Map String Int
upasArray = M.fromList $ (\(a,b)->(b,a)) <$> upasRankList
emptyUpa = ""
emptyUpaNum = 0

sanadis :: [String]
sanadis = ["णिच्","सन्","णिच्†सन्","यङ्","यङ्लुक्"]

--allKrits = ["अच्", "अनीयर्", "अप्", "क", "क्त", "क्तवतुँ", "क्त्वा", "क्यप्", "घञ्", "ड", "ण", "ण्यत्", "ण्वुल्", "तव्य", "तुमुँन्", "तृच्", "यत्", "ल्यप्", "ल्युट्", "श", "शतृँ", "शानच्"]

krits :: [String]
krits = ["घञ्", "अनीयर्", "क्त", "क्तवतुँ", "क्त्वा", "ण्वुल्", "तव्य", "तुमुँन्", "तृच्", "यत्", "ल्यप्", "ल्युट्", "शतृँ", "शानच्", "तिप्"]

-- Grouping krits based on support and पदान्तवर्ण w.r.t different lingas
krits___ = ["क्त्वा", "तुमुँन्", "ल्यप्"] -- avyayas
kritsA = ["अनीयर्", "क्त", "तव्य", "शानच्", "यत्"]
kritsAh = ["घञ्"]
kritsAm = ["ल्युट्"]
kritsKa = ["ण्वुल्"]
kritsTr = ["तृच्"]
kritsAn = ["शतृँ"]
kritsKta = ["क्त"]

-- -- | Only these krits will go into the Trie. Caution - ल्युट् differs from "अनीयर्" in दशन, दरिद्राण (in 300 dhatus alone). Address it.
kernelKrits :: [String]
kernelKrits = ["तुमुँन्", "क्त्वा", "ल्यप्", "क्त", "ल्युट्", "शतृँ", "शानच्", "घञ्", "ण्वुल्", "यत्"]

-- अप् अतिकरः (कॄ, not कृ)
-- क अतिक्रीडः
-- क्यप् अतिखेयः
-- ड सुह्वः
-- ण स्थायः
-- यत् स्थेयः
-- श प्रतिधमः
-- | These are mainly for scrape-stage purposes, may contain 'unsupported' kritPratyayas
triKrits = ["अनीयर्", "क", "क्त", "क्तवतुँ", "क्यप्", "ड", "ण", "ण्यत्", "ण्वुल्", "तव्य", "तृच्", "यत्", "श", "शतृँ", "शानच्"]
pumKrits = ["अच्","अप्", "घञ्"]
napumKrits = ["ल्युट्"]
avyayas = ["क्त्वा", "तुमुँन्", "ल्यप्", "तिप्"] -- Adding तिप् here but it's not used

triKritsEndingA = ["अनीयर्", "क", "क्त", "क्यप्", "ड", "ण", "ण्यत्", "ण्वुल्", "तव्य", "यत्", "श", "शानच्"]
triKritsEndingAa = ["तृच्"]
triKritsEndingAn = ["शतृँ"]
triKritsEndingVaan = ["क्तवतुँ"]

kritsCanon = canon <$> krits
kritsRank = M.fromList $ zip kritsCanon [0::Int ..]
kritsArray = M.fromList $ zip [0::Int ..] kritsCanon

[shatrRank,ghanRank,lyutRank,nvulRank,ktaRank,tumunRank,ktvaRank,lyapRank,tinRank] = (kritsRank M.!) . canon <$>
  ["शतृँ","घञ्","ल्युट्","ण्वुल्","क्त","तुमुँन्","क्त्वा","ल्यप्","तिप्"]
triKernalRanks = (kritsRank M.!) . canon <$> (triKrits `L.intersect` kernelKrits)

-- Kartari: ["ण्वुल्", "तृच्", "शतृँ", "शानच्"]
allPratyayas = S.fromList $ upasargas ++ sanadis ++ krits

plus :: String
plus = "†"

goodSanadis = ["", "णिच्"]

purvopa = upasargas L.\\ ["अपि","अव","आङ्"]
uttaropa = upasargas L.\\ ["अपि","दुस्","सम्"]

practicalize u  -- TODO: The upasargas list should keep the practical version by default.
  | u == "आङ्" = "आ"
  | u == "निस्" = "निः"
  | u == "दुस्" = "दुः"
  | otherwise = u

theorize u
  | u == "आ" = "आङ्"
  | u == "निः" = "निस्"
  | u == "दुः" = "दुस्"
  | otherwise = u
