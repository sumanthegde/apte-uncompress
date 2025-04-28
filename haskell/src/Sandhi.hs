module Sandhi(
  sandhi
  , sandhiPrint
  , sandhiApte
  , sandhiAptePrint
)
where

-- | This module was originally written to serve [upasarga + x] type of sandhi's
--   A wrapper over it, the function `sandhiApte` has been added now, so as to be able to
--   correctly form the compound words grouped under an entry in the Apte dictionary
--
--   This module also assumes:
--   1. devanagari input but with a twist: रआमअ for राम etc
--   2. spaces are removed. (Matters in Visarga lopa) (TODO: check if conflicts with अतएव kind)
--

import Data.List (stripPrefix)
import Data.Maybe
import qualified Data.List as L
import SEncode
import Utils
import Pratyayas

--vowsigns = ['ऄ','ा','ि','ी','ु','ू','ृ','ॄ','ॅ','ॆ','े','ै','ॉ','ॊ','ो','ौ','ॢ','ॣ'] -- "ािीुूृॄॅॆेैॉॊोौॢॣ"
--sonants = "कखगघङचछजझञटठडढणतथदधनऩपफबभमयरऱलळऴवशषसह"
--vowels = "अआइईउऊऋॠएऐओऔ"

--    अच् सन्धयः
ak a = a `elem` "अआइईउऊऋॠ"
deergha a = a `elem` "आईऊॠएऐओऔ"
deerghify = go where
  go 'अ' = 'आ'
  go 'इ' = 'ई'
  go 'उ' = 'ऊ'
  go 'ऋ' = 'ॠ'
  go x = x
savarna a b = let
  hasBoth a1 b1 as = a1 `elem` as && b1 `elem` as
  savarnas = ["अआ","इई","उऊ","ऋॠ","ए", "ऐ", "ओ", "औ"]
  in any (hasBoth a b) savarnas
varnas a
  | a `elem` "अआ" = "अआ"
  | a `elem` "इई" = "इई"
  | a `elem` "उऊ" = "उऊ"
  | a `elem` "ऋॠ" = "ऋॠ"
  | otherwise = ""
at a = a `elem` "अआ"
saSwara cs = (not.null.tail) cs && (head.tail) cs `elem` vowels
enAr ys = any (`L.isPrefixOf` ys) [ "ए","ओ"] || (any (`L.isPrefixOf` ys) (hack_6_1_91 ["अर", "अल"]) && (not.saSwara) (tail ys))
hack_6_1_91 = ("आर" :)  -- उपसर्गादृति धातौ, "प्रार्च्छति"
changePrefix s oldPre newPre = newPre ++ fromJust (stripPrefix oldPre s)

deGuna :: String -> [String]
deGuna a
  | "ए"  `L.isPrefixOf` a = changePrefix a "ए" <$> ["इ","ई"]
  | "ओ" `L.isPrefixOf` a = changePrefix a "ओ" <$> ["उ","ऊ"]
  | "अर" `L.isPrefixOf` a = changePrefix a "अर" <$> ["ऋ","ॠ"]
  | "अल" `L.isPrefixOf` a = changePrefix a "अल" <$> ["ऌ","ॡ"]
  | otherwise = []

aicAar ys = any (ys `headIn`) ["ऐ", "औ"]
deVRddhi a
  | "ऐ"  `L.isPrefixOf` a = changePrefix a "ऐ" <$> ["ए","ऐ"]
  | "औ" `L.isPrefixOf` a = changePrefix a "औ" <$> ["ओ","औ"]
  | otherwise = []

en a = a `elem` "एओ"
pluta a = a == "ऽ"
gunafy a
  | a `elem` "इई" = "ए"
  | a `elem` "उऊ" = "ओ"
  | a `elem` "ऋॠ" = "अर"
--  | a `elem` "ऌॡ" = "अल"  -- enable only if need be [true,["अ","आलयः",["अलयः","आलयः"],["आलअयअः"]]]

ik a = a `elem` "इईउऊऋॠ"
yanify a
  | a `elem` "इई" = 'य'
  | a `elem` "उऊ" = 'व'
  | a `elem` "ऋॠ" = 'र'
--  | a `elem` "ऌॡ" = 'ल'
deYan a = ""

eco :: Char -> String -> [String]
eco c ys = [ys' | ys' `headIn` (if en c then filter (/='अ') else id) vowels] where
  tailIf :: Char -> String -> String
  tailIf c (x:xs) = if c==x then xs else (x:xs)
  tailIf _ xs = xs
  ys' = tailIf (yava c) ys
  yava c = if c `elem` "एऐ" then 'य' else 'व'

-- | For any vowel a, function acVig a C gives all possible strings B such that a+B=C is a valid sandhi (samhita).
--   For now, universally allowing अ + ऋ = [अर्, आर्], despite the आर् option only true for प्र + ऋच्छति  types. (उपसर्गादृति धातौ)
acSandhi :: String -> String -> String
acSandhi purva uttara@(u:rest) = if u `notElem` vowels then purva++uttara else init purva ++ go where
  purvaIsUpa = purva `elem` fmap (canon . practicalize) upasargas
  c = last purva
  go
    | ak c && savarna c u = deerghify c : rest
    | at c && u == 'ऋ' && purvaIsUpa = "आर" ++ rest
    | at c && ik u = gunafy u ++ rest
    | at c && u `elem` "एओ" && purvaIsUpa = uttara
    | at c && u `elem` "एऐ" = 'ऐ':rest
    | at c && u `elem` "ओऔ" = 'औ':rest
    | ik c = yanify c : uttara
    | en c && u == 'अ' = c:'ऽ':rest
    | c == 'ए' = "अय" ++ uttara
    | c == 'ऐ' = "आय" ++ uttara
    | c == 'ओ' = "अव" ++ uttara
    | c == 'औ' = "आव" ++ uttara

nasika = "ञमङणन"
hash = "हयवरल" ++ nasika ++ jhash
jash = "जबगडद"
jhash = "झभ" ++ "घढध" ++ jash
khay = "कखचछटठतथपफ"
khar = khay ++ "शषस"
khal = khay ++ "शषसह"
jhal = jhash ++ khal
-- हल् सन्धयः
-- | Limited cases handled, just to support त्, द्, न्, म्, ं  (उत्, सम् )
-- Update: श्चुत्व being supported partially (ref. श्चुत्व of SandhiVigraha -- "राज्ञः" -- though ended up unused lol)
halSandhi :: Char -> String -> String
halSandhi = go where
  am = vowels ++ "हयवरल" ++ nasika
  go 'त' uttara
    | uttara `headIn` "ह" = "दध" ++ (tail uttara)
    | uttara `headIn` (vowels ++ "गघदधबभयरव") = 'द':uttara
    | uttara `headIn` "ल" = "लल" ++ (tail uttara) -- तोर्लि   (This & many below are Vikalpa but feels good)
    | uttara `headIn` "डढ" = 'ड':uttara
    | uttara `headIn` "टठ" = 'ट':uttara --
    | uttara `headIn` "च" = 'च':uttara
    | uttara `headIn` "छश" = "चछ"++ tail uttara
    | uttara `headIn` "जझ" = 'ज':uttara
    | uttara `headIn` nasika = 'न':uttara -- ToDo Handle न -> ण change too
    | otherwise = 'त':uttara
  go 'म' uttara
    | uttara `elem` ["चइत","चअनअ"] = 'ञ':uttara -- Caution: Ugly hack to support किञ्चन etc
--    | uttara `headIn` "कखगघङ" = 'ङ':uttara
--    | uttara `headIn` "चछजझञ" = 'ञ':uttara
--    | uttara `headIn` "टठडढण" = 'ण':uttara
--    | uttara `headIn` "तथदधन" || "हन" `L.isPrefixOf` uttara = 'न':uttara
--    | uttara `headIn` "यरलवशषसह" = 'ं':uttara
    | uttara `headIn` sonants = 'ं':uttara  -- Caution: Assumed that purvapada is always सं (upasarga)
    | otherwise = 'म':uttara
  go 'ज' uttara
    | uttara `headIn` "न" = "जञ"++tail uttara
    | otherwise = 'ज':uttara
  go 'न' uttara  -- , 
    | uttara `headIn` vowels = "नन" ++ uttara -- 8/3/32 ङमो
    | uttara `headIn` "चछ" && tail uttara `headIn` am = "ंश" ++ uttara -- नश्छव्यप्रशान्
    | uttara `headIn` "टठ" && tail uttara `headIn` am = "ंष" ++ uttara -- नश्छव्यप्रशान्
    | uttara `headIn` "तथ" && tail uttara `headIn` am = "ंस" ++ uttara -- नश्छव्यप्रशान्
    | uttara `headIn` "जझ" = "ञ" ++ uttara -- श्चुत्व
    | otherwise = 'न':uttara
  go _ _ = []
--sonants = "कखगघङचछजझञटठडढणतथदधनऩपफबभमयरऱलळऴवशषसह"


-- विसर्ग-सन्धयः
-- | Produces दुःसाह (वा शरि), निःक्षञ्ज all of which the Vigraha module may not be able to reverse
-- TODO: Make Sandhi and Vigraha modules overlap completely
visargaSandhi :: Char -> String -> String
visargaSandhi penult uttara@(u:rest)
  | penult == 'अ' && u == 'अ' = "ओऽ" ++ rest
  | penult == 'अ' && u `elem` hash = 'ओ' : uttara
  | at penult && u `elem` vowels = penult:uttara
  | penult == 'आ' && u `elem` hash = 'आ':uttara
  | penult `elem` "इउ" && u == 'र' = deerghify penult :uttara -- 6/3/111 dumbed down. Gets निः+रसः, misses पुनः+रचनम्
  | u `elem` (vowels ++ hash) = penult:'र':uttara
  | u `elem` "शषस" || (u `elem` khay && rest `headIn` "शषस") = penult:'ः':uttara -- Only वा शरि considered.
  | u `elem` "चछ" = penult:'श':uttara
  | u `elem` "टठ" = penult:'ष':uttara
  | u `elem` "तथ" = penult:'स':uttara
  | penult `elem` "इईउऊ" && u `elem` "कखपफ" = penult:'ष':uttara -- Todo: ban it unless for निस् or दुस् (इदुदुपधस्य)
  | otherwise = penult:'ः':uttara

-- | Does sandhi of purva pada (likely an upasarga) and uttara pada
--   Caution: First arg is assumed to be of length ≥ 2 if it ends with ः. Also, all in fullVow'' form of course.
--   Caution: Sandhi result is assumed to be just one possibility (& one word, like अतएव). May have to address multiplicity later.
sandhi :: String -> String -> String
sandhi purva "" = purva
sandhi purvaPada uttaraPada@(c:rest)
  | null purvaPada = uttaraPada
  | purvaPada `elem` ["सअः","एषअः"] && c /= 'अ' = init purvaPada ++ uttaraPada
  | purvaCh == 'ः' = pre2 ++ visargaSandhi (head suf2) uttaraPada
  | purvaCh `elem` vowels = acSandhi purvaPada uttaraPada
  | otherwise = pre1 ++ halSandhi purvaCh uttaraPada
  where
    purvaCh = last purvaPada
    (pre1,suf1) = splitAt (length purvaPada - 1) purvaPada
    (pre2,suf2) = splitAt (length purvaPada - 2) purvaPada

sandhiPrint :: String -> String -> String
sandhiPrint purva uttara = uncanon $ canon purva `sandhi` canon uttara
--कखगघङचछजझञटठडढणतथदधनऩपफबभमयरऱलळऴवशषसह

swap = \(a,b)->(b,a)
jashtva = zip "कचटतप" "गजडदब"
chartva = swap <$> jashtva
alpapranatva = zip "खघछझठढथधफभ" "कगचजटडतदपब"
anunasikatva = zip "गजडदब" "ङञणनम"
mahapranatva = swap <$> alpapranatva
kutva = zip "चछजझञ" "कखगघङ"
torli = zip "तदन" "ललल"
shtutva = zip "सतथदधन" "षटठडढण"
shchutva = zip "सतथदधन" "शचछजझञ"
anusvaratva = [('म','ं')]
dhatva = [('ह','ढ')]
-- uttarapada-based purva adesha/agamas: rutva, anunasika. (not applicable in apte)
-- uttarapada-based uttara adesha: ho jhashtva, shah chatva
-- purvapada-based uttara agama: cha tugagama (if hrasva purva)
-- two-letters-purva-based: cha tugagama, ङमुडागम (unless न)
-- स्तु(श्चु) श्चु
-- स्तु(ष्टु) ष्टु
-- च श(छ)
-- जश् ह(झष्)
-- म्(ङम्) ञय्
--
subject xmap x = fromMaybe x (x `lookup` xmap)
subjectOn cond xmap x = if cond then subject xmap x else x
(.>) = flip (.)


-- | In Apte, म् changes to anusvara
halSandhiApte :: Char -> String -> String
halSandhiApte x' (y:ys) = doUttara (doPurva x') where
  doPurva =     subject kutva
             .> subject alpapranatva
             .> subjectOn (y `notElem` khar) jashtva -- approx for: jashtva on padanta & jhal
             .> subjectOn (y `elem` khar) chartva
             .> subjectOn (y `elem` "ल") torli
             .> subjectOn (y `elem` nasika) anunasikatva
             .> subjectOn (y `elem` "षटठडढण") shtutva
             .> subjectOn (y `elem` "शचछजझञ") shchutva
             .> subjectOn (y `notElem` vowelsReal) anusvaratva
             -- todo: Also add special cases like अहन्, महा etc
  doUttara x
    | x `elem` jash && y == 'ह' = x: subject mahapranatva x: ys
    | [x,y] == "चश"             = "चछ" ++ ys
    | otherwise                 = x:y:ys


handleRepha :: String -> String -> String
handleRepha purvaInit uttara
  | last purvaInit `notElem` "अआ" || head uttara `elem` khar = sandhi (purvaInit ++ "ः") uttara -- खर...विसर्जनीयः
  | head uttara == 'र' = init purvaInit ++ [deerghify $ last purvaInit] ++ uttara -- रो रि
  | otherwise = purvaInit ++ "र" ++ uttara


vasu pada = uncanon pada `elem` ["विद्वस्","स्रस्","ध्वस्","अनडुह्"]

-- | Cases where स् + क/प cannot be treated as ः + क/प. Approximate code.
kaskAdi :: String -> String -> Bool
kaskAdi purva uttara = let
  kaska = canon <$> ["अयस्","अहस्","तमस्","भास्","मेदस्"] -- 8/3/48
  kaska2 = canon <$> ["तिरस्","नमस्", "पुरस्"] -- 8/3/40,41
  purvas = kaska ++ kaska2
  apapada = canon <$> ["पाश","कल्प","क","काम्य"] -- 8/3/38
  atah = canon <$> ["कार","कृत्","काम","कंस","कुंभ","कुशा","कर्णी"] -- 8/3/46. कुम्भ -> कुंभ for Apte
  uttaras = apapada ++ atah
  in (any (`L.isSuffixOf` purva) purvas && uttara `headIn` "कखपफ")
     || (takeEnd' 2 purva == "अस" && any (`L.isPrefixOf` uttara) uttaras)
     || purva == canon "पुंस्" && uttara `headIn` "कखपफ" -- rest of 8/3/6 handled by upstream. todo: combine checks in one place

anch pada = takeEnd' 2 pada `elem` ["ञच", "ंच"]

-- | Stems whose last letter is श/ष/ह but becomes क. See स्पृश्… 3/2/58-61, क्विन्… 8/2/62, दा… 8/2/32
--   Suffix match to catch तादृश् etc
kvinKvip pada = any ((`L.isSuffixOf` pada).canon) ["स्पृश्", "दधृष्", "दिश्", "उष्णिह्", "दृश्", "दुह्", "द्रुह्"]

-- | Stems whose last letter is in ज,श,ष,ह but becomes ट eventually.
--   Relevant rules: ज/श->ष (व्रश्च… 8/2/36),  ष->ड (जश्), हो ढः.
--   Assumption: क्विन्,क्विप् cases have been addressed already
--   Suffix match to catch विराज्, परिव्राज्(!)
qatvaarha pada = last pada `elem` "शषह" || any ((`L.isSuffixOf` pada).canon) ["व्रश्च्", "भ्रस्ज्", "सृज्", "मृज्", "यज्", "राज्", "भ्राज्"]

-- | Only meant for sandhi as part of samasa of words as specified in Apte dictionary. Thus,
--   1. न+... undergoes नलोप instead of रुँत्व, ङमुडागम etc
--   2. anusvara preferred to anunasika
sandhiApte :: String -> String -> String
sandhiApte purva "" = purva
sandhiApte "" uttara = uttara
sandhiApte (c:"") uttara = sandhi (c:"") uttara
sandhiApte purva uttara
  | last purva == 'न' = sandhiApte (init purva) uttara -- नलोप
  | last purva == 'र' = handleRepha (init purva) uttara -- रो रि
  | vasu purva = init purva ++ halSandhiApte 'त' uttara -- विद्वस् … 8/2/72
  | kaskAdi purva uttara = purva ++ uttara
  | last purva == 'स' = sandhi (init purva ++ "ः") uttara
  | anch purva = init (init purva) ++ halSandhiApte 'क' uttara -- 6/4/24 e.g. सम्यञ्च् (6/3/93)
  | kvinKvip purva = init purva ++ halSandhiApte 'क' uttara --    क्विन्->कः
  | qatvaarha purva = init purva ++ halSandhiApte 'ट' uttara
  | last purva `elem` "अइउऋ" && head uttara == 'छ' = purva ++ "च" ++ uttara -- तुगागम+श्चुत्व
  | last purva `elem` sonants = init purva ++ halSandhiApte (last purva) uttara
  | otherwise = sandhi purva uttara

sandhiAptePrint :: String -> String -> String
sandhiAptePrint purva uttara = uncanon $ canon purva `sandhiApte` canon uttara