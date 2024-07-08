{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, DeriveGeneric, TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module ApteRecords where

import qualified Data.ByteString.Char8 as BS8
import Data.Aeson
import qualified GHC.Generics as G
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import qualified Control.Monad as CM
import GHC.Base ((<|>), Alternative)
import Control.Lens
type BS8B = BS8.ByteString

type LNumber = String
type AfterL' = String
type Step0 = M.Map LNumber AfterL'

-- https://stackoverflow.com/a/22813594/10167011
class OpR a where
    liftR2 :: (forall x. Maybe x -> Maybe x -> Maybe x) -> a -> a -> a

instance (G.Generic a, GenOpR (G.Rep a)) => OpR a where
    liftR2 op a b = G.to (genliftR2 op (G.from a) (G.from b))

class GenOpR f where
    genliftR2 :: (forall x. Maybe x -> Maybe x -> Maybe x) -> f a -> f a -> f a

instance GenOpR a => GenOpR (G.M1 i c a) where
    genliftR2 op (G.M1 a) (G.M1 b) = G.M1 (genliftR2 op a b)

instance (GenOpR a, GenOpR b) => GenOpR (a G.:*: b) where
    genliftR2 op (al G.:*: bl) (ar G.:*: br) = genliftR2 op al ar G.:*: genliftR2 op bl br

instance GenOpR (G.K1 i (Maybe a)) where
    genliftR2 op (G.K1 a) (G.K1 b) = G.K1 (op a b)

data Location = L_ {loc :: String}
             | B_ {loc :: String} -- दुर्, त्रि, नृ
             | M_ {loc :: String} --
             | S_ {loc :: String} -- दुर्-गः, त्रि-पुरं, नृ-पः
             | S_M_ {loc :: String} -- दुर्-ग (--गः), त्रि-पुरं (--रः),  किं -नर (--री) NOT किं -नर ॰ईश्वरः (--री)
             | S_M_S_ {loc :: String} -- दुर् -ग (--गः) ॰कारक, क्षण -द (--दा) ॰करः
             | S_M_S_M_ {loc :: String} -- दुर् -ग ॰कारक (-कः) NOT  दुर् -ग (-कः)!?
             | S_S_ {loc :: String} -- नृ -प ॰वल्लभः, किं -नर ॰ईश्वरः
             | S_S_M_ {loc :: String} -- नृ -प ॰वल्लभः (--भा)
             deriving (G.Generic, Eq, Ord)
instance Show Location where
  show = loc

showLoc :: Location -> String
showLoc = go where
    makePath label val = label ++ " " ++ val
    go (L_ x) = makePath "L_" x
    go (B_ x) = makePath "B_" x
    go (M_ x) = makePath "M_" x
    go (S_ x) = makePath "S_" x
    go (S_M_ x) = makePath "S_M_" x
    go (S_M_S_ x) = makePath "S_M_S_" x
    go (S_M_S_M_ x) = makePath "S_M_S_M_" x
    go (S_S_ x) = makePath "S_S_" x
    go (S_S_M_ x) = makePath "S_S_M_" x
    
showLocs :: [Location] -> String
showLocs = L.intercalate " / " . fmap showLoc 

instance ToJSON Location where
  toJSON = toJSON . showLoc

instance FromJSON Location where
    parseJSON = withText "Location" $ \t ->
        let str = T.unpack t
        in case str of
            _ | Just val <- L.stripPrefix "L_ " str -> return $ L_ val
              | Just val <- L.stripPrefix "B_ " str -> return $ B_ val
              | Just val <- L.stripPrefix "M_ " str -> return $ M_ val
              | Just val <- L.stripPrefix "S_ " str -> return $ S_ val
              | Just val <- L.stripPrefix "S_M_ " str -> return $ S_M_ val
              | Just val <- L.stripPrefix "S_M_S_ " str -> return $ S_M_S_ val
              | Just val <- L.stripPrefix "S_M_S_M_ " str -> return $ S_M_S_M_ val
              | Just val <- L.stripPrefix "S_S_ " str -> return $ S_S_ val
              | Just val <- L.stripPrefix "S_S_M_ " str -> return $ S_S_M_ val
              | otherwise -> CM.mzero  -- Fail parsing if no prefix matches

instance ToJSONKey Location
instance FromJSONKey Location

-- Term is the basic unit of the dictionary: Essentially, it's a title (aka _banner) along with the meaning list.
-- Attributes _morphisms & _samasas are added to capture the recursive structure.
-- By morphism we mean a minor change to title (e.g. gender) but possibly having a different meaning.
-- _samasas are self evident.
-- _gram captures grammatical info given before starting the meaning list.
-- Other attributes do bookkeeping.
-- __line captures the line number of the term in ap90.txt 
-- _bannerExp is populated with "full compound word" while _banner stores the original shorthand.
-- _ancestry stores "address", i.e. titles of enclosing terms.
data Term = Term
  { __line :: Maybe Int
  , _ancestry :: Maybe [Location]
  , _banner :: Maybe String
  , _bannerExp :: Maybe [Either String String]
  , _gram :: Maybe [String]
  , _meanings :: Maybe [String]
  , _morphisms :: Maybe [Term]
  , _samasas :: Maybe [Term] -- After --Comp though, sub-samasa precedes morphisms (each with potentially their own subsamasas)
  , _spent :: Maybe String
  , _unspent :: Maybe String
  }
  deriving (G.Generic,Show)
makeLenses ''Term

instance ToJSON Term where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}
instance FromJSON Term where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

-- Same explanation as in Term holds here, for Left, Right & Nothing.
data Record = Record
  { _k1 :: Maybe String
  , _k2 :: Maybe String
  , _term :: Maybe Term
  }
  deriving (G.Generic,Show)
makeLenses ''Record
instance ToJSON Record where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}
instance FromJSON Record where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}
type RecordM = M.Map String Record

fromValidJSON :: FromJSON a => Value -> a
fromValidJSON v = case fromJSON v of
  Success t -> t
  _ -> error "Failed to decode"

termNil = fromValidJSON (object []) :: Term
recordNil = fromValidJSON (object []) :: Record
recordMNil = M.empty :: RecordM

patchTerm :: Term -> Term -> Term
patchTerm = liftR2 (<|>)

maskRecordM :: RecordM -> RecordM -> RecordM
maskRecordM = M.unionWith (liftR2 (>>))

patchRecord :: RecordM -> RecordM -> RecordM
patchRecord = M.unionWith (liftR2 (<|>))

data Envt = Envt
  { -- address in the tree
    locations :: [Location]
    -- allows starting from "3" for new entry, in: {1} <meaning> {2} <new entry> <meaning> {3} <meaning>
  , meaningNot1 :: [([Location],Int)]
    -- allows meaning 2 to be empty, in: {1} <meaning> {2} <new entry> <meaning>
  , meaningEmpty :: [[Location]]
    -- helper function in ApteUtils can populate subsamasaMorphismLocs, using syl-compat logic
  , subsamasaMorphismLocs :: [[Location]]
    -- total number of lines in the L block currently under process
  , nLines :: Int
    -- line number of every <L> tag in the original ap90.txt
  , mapLnu :: M.Map String Int
  }
  deriving (G.Generic, Show)
instance ToJSON Envt
instance FromJSON Envt
