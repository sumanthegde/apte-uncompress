module Utils where
-- Frequently used data/functions by other modules is kept here
    
import qualified Data.Map as M
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as E
import qualified Data.List as L
import qualified Data.Set as S
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)
import System.IO (stdout, stderr, Handle, hPutStrLn)

sigMap = [('अ','ऄ'),('आ','ा'),('इ','ि'),('ई','ी'),('उ','ु'),('ऊ','ू'),('ऋ','ृ'),('ॠ','ॄ'),('ऌ','ॢ'),('ॡ','ॣ'),
          ('ऍ','ॅ'),('ऎ','ॆ'),('ए','े'),('ऐ','ै'),('ऑ','ॉ'),('ऒ','ॊ'),('ओ','ो'),('औ','ौ')]
sonants = "कखगघङचछजझञटठडढणतथदधनऩपफबभमयरऱलळऴवशषसह"
vowels = map fst sigMap
vowelsReal = "अआइईउऊऋॠऌएऐओऔ"
 -- "अआइईउऊऋॠऌॡऍऎएऐऑऒओऔ"
vowSigns = map snd $ tail sigMap -- ['ा','ि','ी','ु','ू','ृ','ॄ','ॢ','ॣ','ॅ','ॆ','े','ै','ॉ','ॊ','ो','ौ'] -- "ािीुूृॄॅॆेैॉॊोौॢॣ"

headIn :: String -> [Char] -> Bool
headIn = go where
  go (c:cs) ts = c `elem` ts
  go _ _ = False

printuH :: A.ToJSON p => Handle -> p -> IO ()
printuH h obj = do
  let j = E.encodePretty obj
  BL.hPutStr h j
  hPutStrLn h "\n"

tracePrintuH :: A.ToJSON p => Handle -> p -> a -> a
tracePrintuH h msg ret = unsafePerformIO (do {printuH h msg; return ret;})

tracePrintuHI :: A.ToJSON p => Handle -> p -> a -> a
tracePrintuHI h msg ret = unsafePerformIO (do {printuH h msg; getChar; return ret;})

printu p = printuH stdout p
printuE p = printuH stderr p
tracePrintu p = tracePrintuH stdout p
tracePrintuI p = tracePrintuHI stdout p
tracePrintuE p = tracePrintuH stderr p
tracePrintuf f x = tracePrintu (f x) x

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList
  
maybeReplaceSuf newSuf oldSuf str = let
  its = zip (L.inits str) (L.tails str)
  before = L.dropWhile ((/=oldSuf).snd) its
  in if null before then Nothing else Just $ (fst $ head before) ++ newSuf

replaceSuf newSuf oldSuf str = fromMaybe str $ maybeReplaceSuf newSuf oldSuf str

removeSuf = replaceSuf ""
removeOneSuf = go where
   go [] x = x
   go (suf:rest) x = if suf `L.isSuffixOf` x then removeSuf suf x else go rest x

testReplaceSuf = 
   replaceSuf "xxx" "xxxx" "abc"           ==   "abc" 
   && replaceSuf "xxx" "xxxx" "abcxxx"     ==   "abcxxx"
   && replaceSuf "xxx" "xxxx" "abcxxxx"    ==   "abcxxx"
   && replaceSuf "xxx" "xxxx" "abcxxxxx"   ==   "abcxxxx"
   && replaceSuf "xxx" "xxxx" "abcxxxxyx"  ==   "abcxxxxyx"
   && replaceSuf "xxx" "xxxx" "abcxxxyxx"  ==   "abcxxxyxx"
   && replaceSuf "xxx" "xxxx" "abcxxyxxx"  ==   "abcxxyxxx"
   && replaceSuf "xxx" "xxxx" "abcxyxxxx"  ==   "abcxyxxx"
   && replaceSuf "xxx" "" "abcxyxxxx"      ==   "abcxyxxxxxxx"
   && replaceSuf "" "" "abcxyxxxx"         ==   "abcxyxxxx"
   && replaceSuf "t" "" "abcxyxxxx"        ==   "abcxyxxxxt"
   && replaceSuf "" "t" "abcxyxxxx"        ==   "abcxyxxxx"
   && replaceSuf "" "xx" "abcxyxxxx"       ==   "abcxyxx"
   
twoPow i = if i<63 then 2^i else undefined
bitmask :: [Int] -> Int
bitmask is = sum $ map twoPow is

unbitmask :: Int -> [Int]
unbitmask n = reverse (go n 0 []) where
  go 0 _ ans = ans
  go n' j ans = go (div n' 2) (j+1) (if mod n' 2 == 1 then j:ans else ans)

sans="अआइईउऊ"++"ऋॠऌॡ"++"एऐओऔंः"++"ँ"++"कखगघङ"++"चछजझञ"++"टठडढण"++ "तथदधनपफबभमयरलव"++"शषसहऽळ"
eng= "aAiIuU"++"fFxX"++"eEoOMH" ++"~"++"kKgGN" ++"cCjJY" ++"wWqQR"++"tTdDnpPbBmyrlv"++"Szsh'L"
s2eMap = M.fromList $ sans `zip` eng
e2sMap = M.fromList $ eng `zip` sans
s2e = map (s2eMap M.!)
e2s = map (e2sMap M.!)

fst3 = (\(a1,a2,a3)->a1)
snd3 = (\(a1,a2,a3)->a2)
trd3 = (\(a1,a2,a3)->a3)

fst4 = (\(a,_,_,_)->a)
snd4 = (\(_,a,_,_)->a)
trd4 = (\(_,_,a,_)->a)
frh4 = (\(_,_,_,a)->a)

takeEnd' k = reverse . take k . reverse

apteDir = "apteDir.nosync"
originalPath = apteDir </> "ap90/txt/ap90.txt"
ap90 = apteDir </> "patches"
initialEnvt = apteDir </> "envt.json"
apteOutput = apteDir </> "output"
dsalDir = apteDir </> "dsal"
inlineTxtPath = apteOutput </> "inline.txt"
rec0Path = apteOutput </> "0.json"
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