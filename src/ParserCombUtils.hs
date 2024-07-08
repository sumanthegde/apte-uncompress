module ParserCombUtils where

import qualified Text.ParserCombinators.ReadP as R
import Control.Monad
import Data.Char
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import Data.Maybe
import Data.Aeson
import SEncode (canon)
import Data.Functor ((<&>))

parse = R.readP_to_S

lit = R.string
or2 = liftM2 (||)

satisfyAny :: [Char -> Bool] -> R.ReadP Char
satisfyAny conds = R.satisfy (foldr1 or2 conds)

-- | Returns all possible matches surrounded by open and close
surr :: String -> String -> R.ReadP a -> R.ReadP a
surr open close = R.between (lit open) (lit close)

-- | Returns shortest possible match surrounded by open and close
surrLazy :: String -> String -> R.ReadP String
surrLazy open close = R.between (lit open) (lit close) (superstringOfNoneOf [close])

aheadSatisfy p = do
  cs <- R.look
  guard $ p cs
  return ()

getIfRestSatisfy p = do
  cs <- R.look
  guard $ p cs
  R.get

lookAhead pref = aheadSatisfy (pref `L.isPrefixOf`)
prefixOfNoneOf bads input =  not $ any (`L.isPrefixOf` input) bads
-- -- | Todo: Deprecate it in favor of superstringOfNoneOf'
superstringOfNoneOf bads = R.many1 $ getIfRestSatisfy $ prefixOfNoneOf bads
superstringOfNoneOf' bads = manyGreedy1 $ getIfRestSatisfy $ prefixOfNoneOf bads

pConcat :: Char -> R.ReadP (String -> String -> String)
pConcat c = R.char c >> return (\xs ys -> xs ++ (c:ys))

-- | Given an operator and a list, parse tokens as per parsers in the list, delimited by the operator
--   Example: chains (pConcat ' ') [dateParser,timeParser] "2000-01-01 00:00:00"
--   Useful to validate the string rather than to extract tokens out of it
chains :: R.ReadP (a -> a -> a) -> [R.ReadP a] -> R.ReadP a
chains = go where
  go op [p] = p
  go op (p:ps)  = do
        cs <- p
        o <- op
        rest <- go op ps
        return (cs `o` rest)

-- | Given a parser for delimiter and a list of parsers, parse the tokens in the same order as the list.
chainSeq :: R.ReadP a -> [R.ReadP a] -> R.ReadP [a]
chainSeq delim ps = do
  as <- sequence (L.intersperse delim ps)
  let alt (x:_:xs) = x: alt xs; alt ys = ys
  return (alt as)

pJoin :: [R.ReadP String] -> R.ReadP String
pJoin = chains (pure (++))

-- | Like R.many, but gets the longest match. Caution: Beware of matching empty string indefinitely.
manyGreedy :: R.ReadP a -> R.ReadP [a]
manyGreedy p = liftM2 (:) p (manyGreedy p) R.<++ pure []

-- | Parse tokens from as per the parser list, as far as possible.
chainMaximal :: [R.ReadP a] -> R.ReadP [a]
chainMaximal = go where
  go [] = pure []
  go (p:ps) = liftM2 (:) p (go ps) R.<++ pure []

guard' :: (a->Bool) -> R.ReadP a -> R.ReadP a
guard' cond p = do
  x <- p
  guard (cond x)
  return x

manyGreedy1 :: R.ReadP a -> R.ReadP [a]
manyGreedy1 = guard' (not.null) . manyGreedy

chainMaximal1 :: [R.ReadP a] -> R.ReadP [a]
chainMaximal1 = guard' (not.null) . chainMaximal

pInt = R.munch1 isDigit
pDecimal = pJoin [R.string ".", pInt] R.<++ pure ""
pNumber = pJoin [pInt, pDecimal]
pDate = chains hyp (3 `replicate` pInt) where hyp = (pConcat '-')
pTime = chains col [chains col (2 `replicate` pInt), pNumber] where [dot,col] = pConcat <$> ".:"
pDateTime = chains sp [pDate, pTime] where sp = pConcat ' '

--pFirstLine = chains (lit " ") [pDate, pTime]