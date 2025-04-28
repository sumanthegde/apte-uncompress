module Natva where

-- | Tells if the word has a suffix such that appending "न.." to it would have to undergo णत्व
natvaNext :: String -> Bool
natvaNext = flip go False where
  causer = 'ृ':'ॄ':"ॠऋरष"  -- Caution: As of now, it's used in both pre-canon and canon versions
  breaker = "चछजझञटठडढणतथदधनलशस"
  go (c:cs) b
    | c `elem` causer = go cs True
    | c `elem` breaker = go cs False
    | otherwise = go cs b
  go [] b = b
