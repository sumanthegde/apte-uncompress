{-# LANGUAGE FlexibleInstances #-}

module SEncode (
  canon
  , uncanon
  , dropZWJ
  , yogavahaPosCheck
  , stdsuf
  , stdmid
)where
-- TODO: Test with text with contiguous vowels, like "अतएव". This should be allowed. Similarly, देवऋषिः, महाऋषिः...
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Utils
import Data.Functor ((<&>))
unsigMap = (\(x,y)->(y,x)) <$> sigMap
lookup' x y = fromMaybe x $ L.lookup x y
sigVow1'' = flip lookup' sigMap
unsigVow1'' = flip lookup' unsigMap

-- | 'इ'->'ि' etc (only if 'इ''s predecessor is sonant*)
--   * Recall, we want to allow "अतएव" types from world (outside the app)
--     Hopefully, that's not a problem here coz by now it's converted to अतऄएवऄ
--    Todo - verify it's indeed the case.
unFullVow'' :: [Char] -> [Char]
unFullVow'' xs = let
  sig1 x prev = if prev `elem` sonants then sigVow1'' x else x
  in  zipWith sig1 xs (' ':xs)

fullVow'' :: [Char] -> [Char]
fullVow'' = map unsigVow1''


gra' :: String -> String
gra' (ch:'्':rest) = ch: gra' rest
gra' (ch:rest)
  | ch `elem` sonants && (null rest || head rest `notElem` vowSigns) = ch:'ऄ': gra' rest
  | otherwise = ch: gra' rest
gra' "" = ""

ungra' :: String -> String
ungra' s@(ch:'ऄ':rest) =  ch: ungra' rest
ungra' (ch:rest)
  | ch `elem` sonants && (null rest || head rest `notElem` vowSigns) =   ch:'्': ungra' rest
  | otherwise =  ch: ungra' rest
ungra' "" = ""

canon = fullVow'' . gra'
uncanon = ungra' . unFullVow''

sample = "एकदा त्वं हि हे प्वाल नूनमितो गमिष्यसि, शेषाणि ते स्मरणाणि शीर्णपुष्पगन्धमिव शृणु शृणोति"
f x = writeFile "/opt/kridanta_corpus/jsons/gita2.txt" $ gra' x
g x = writeFile "/opt/kridanta_corpus/jsons/gita3.txt" $ ungra' $ gra' x

put_ xs = putStrLn xs -- $ (intersperse '_' xs) ++ "_"
ff x = putStrLn $ gra' x
gg x = do
  put_ $ gra' x
  put_ $ ungra' (gra' x)

dropZWJ xs = filter (/= '\8205') xs

testGraUngra :: IO ()
testGraUngra = do
  x <- readFile "/opt/kridanta_corpus/jsons/gita.txt"
  let a0 = dropZWJ x
      a1 = gra' a0
      a2 = fullVow'' a1
      a3 = unFullVow'' a2
      a4 = ungra' a3
  writeFile "/opt/kridanta_corpus/jsons/gita0.txt" a0
  writeFile "/opt/kridanta_corpus/jsons/gita1.txt" a1
  writeFile "/opt/kridanta_corpus/jsons/gita2.txt" a2
  writeFile "/opt/kridanta_corpus/jsons/gita3.txt" a3
  writeFile "/opt/kridanta_corpus/jsons/gita4.txt" a4

-- | TODO: Refine the rule to accommodate NiHkShunnaH etc
yogavahaPosCheck :: String -> Bool
yogavahaPosCheck = go where
  go (a:b:cs)
  --  | all (`elem` vowels) [a,b] = False
    | a `notElem` vowels && b `notElem` (vowels++sonants) = False
    | otherwise = go (b:cs)
  go _ = True


testWellFormed = let
  bad = fullVow''.gra' <$> ["य्ँ","य्ं","यःः","यंः"]
  good = fullVow''.gra' <$> ["य्","यं","यँय"]
  in all (not.yogavahaPosCheck) bad && all yogavahaPosCheck good

-- | Standardize the suffix. ं → म् (i.e म, in gra' form)
--   TODO: Reject anomalous input early on, like कर्तुं आगतः, or जाग्रद्प्रगतः
stdsuf :: String -> [String]
stdsuf y = go where
  (i,l) = (init y, last y)
  go
    | l == 'ं' = [i ++ "म"]
    | l == 'ओ' = [i ++ "अः"]
    | l == 'द' = [i ++ "त"] -- TODO handle शरद् etc
    | l == 'ग' = [i ++ "क"]
    | l == 'ग' = [i ++ "क"]
    | l `elem` "रशषस" = [i ++ "ः"]
    | otherwise = [y]

-- | रंग->रङ्ग, नमो॒->नमो, काऽपि->कापि (but कोऽपि as-is), आर्य्य->आर्य
stdmid = go where
  go "" = ""
  go ('ं':c:cs)
    | c `elem` "कघगघङ" = 'ङ': go (c:cs)
    | c `elem` "चछजझञ" = 'ञ': go (c:cs)
    | c `elem` "टठडढण" = 'ण' : go (c:cs)
    | c `elem` "तथदधन" = 'न' : go (c:cs)
    | c `elem` "पफबभम" = 'म' : go (c:cs)
    | otherwise = 'ं' : go (c:cs)
  go ('॒':cs) = go cs
  go ('॑':cs) = go cs
  go ('॓':cs) = go cs
  go (c:'ऽ':cs)
    | c `elem` "एओ" = c: 'ऽ': go cs
    | otherwise = c: go cs 
  go (c:d:e:cs)
    | and ([c,d,e] <&> (`elem` sonants)) && d == e = c:d: go cs -- झरो झरि सवर्णे
    | otherwise = c : go (d:e:cs)
  go (c:cs) = c : go cs

{-
ऀ	ँ	ं	ः	ऄ	अ	आ	इ	ई	उ	ऊ	ऋ	ऌ	ऍ	ऎ	ए
U+091x	ऐ	ऑ	ऒ	ओ	औ	क	ख	ग	घ	ङ	च	छ	ज	झ	ञ	ट
U+092x	ठ	ड	ढ	ण	त	थ	द	ध	न	ऩ	प	फ	ब	भ	म	य
U+093x	र	ऱ	ल	ळ	ऴ	व	श	ष	स	ह	ऺ	ऻ	़	ऽ	ा	ि
U+094x	ी	ु	ू	ृ	ॄ	ॅ	ॆ	े	ै	ॉ	ॊ	ो	ौ	्	ॎ	ॏ
U+095x	ॐ	॑	॒	॓	॔	ॕ	ॖ	ॗ	क़	ख़	ग़	ज़	ड़	ढ़	फ़	य़
U+096x	ॠ	ॡ	ॢ	ॣ	।	॥	०	१	२	३	४	५	६	७	८	९
U+097x	॰	ॱ	ॲ	ॳ	ॴ	ॵ	ॶ	ॷ	ॸ	ॹ	ॺ	ॻ	ॼ	ॽ	ॾ	ॿ
-}