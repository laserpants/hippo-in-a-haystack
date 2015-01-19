{-# LANGUAGE OverloadedStrings #-}
module Hippo.Core
  ( Graph, Schema, keys, construct
  , Dict, DataStore, StoreItem(..)
  , dictKeys, dictFold, emptyDict, dsLookup
  , merge, queryGraph, exQuery
  ) where

import Control.Applicative
import Data.Maybe                ( mapMaybe )
import Data.List.Utils           ( addToAL )
import Data.Maybe                ( fromMaybe )
import Data.Text                 ( Text )

import qualified Data.Map.Strict as M

-- ////////////////////////////////////////////////////////////////////////////
-- Core types
-- ////////////////////////////////////////////////////////////////////////////

data StoreItem = Str Text | Zahl Int | Boolean Bool | Real Double

type Dict   = M.Map Int [Int]
type Graph  = [(Text, Dict)]
type Schema = [(Text, [(Text, Text)])]

emptyDict :: Dict
emptyDict = M.empty

dictKeys :: Dict -> [Int]
dictKeys = M.keys

dictFold :: (Int -> [Int] -> Text) -> Dict -> [Text]
dictFold fn = M.foldrWithKey' ((:) .:. fn) []

(.:.) :: (s -> t) -> (d -> o -> s) -> d -> o -> t
(.:.) = (.).(.)

keys :: Graph -> [Text]
keys = map fst

type DataStore = M.Map Int StoreItem

dsLookup :: Int -> DataStore -> Maybe StoreItem
dsLookup = M.lookup

-- | Construct a Graph from an assoc. list.
construct :: [(Text, [(Int, [Int])])] -> Graph
construct g = fmap M.fromList <$> g

-- ////////////////////////////////////////////////////////////////////////////
-- Merge
-- ////////////////////////////////////////////////////////////////////////////

merge :: Graph   -- ^ Needle
      -> Graph   -- ^ Haystack
      -> Schema  -- ^ Schema
      -> Graph
merge n hs schema = foldr (uncurry f) n n
  where
    f :: Text -> Dict -> Graph -> Graph
    f t d g = foldr (uncurry ins) g $ itemPairs d $ arrows schema t

    ins :: Text -> Int -> Graph -> Graph
    ins t n g = insert' g t n $ lookup' hs t n

    itemPairs :: Dict -> [Text] -> [(Text, Int)]
    itemPairs d ts = concat $ zip ts <$> M.elems d

    lookup' :: Graph -> Text -> Int -> [Int]
    lookup' g t n = fromMaybe [] $ lookup t g >>= M.lookup n

    insert' :: Graph -> Text -> Int -> [Int] -> Graph
    insert' g "@" _ _ = g
    insert' g t n xs  =
        case insMemb g t n xs of
          (True, g') -> f t (M.fromList [(n, xs)]) g'
          (_,    g') -> g'

insMemb :: Graph -> Text -> Int -> [Int] -> (Bool, Graph)
insMemb g t n xs =
    case lookup t g >>= M.lookup n of
      Nothing -> (True,  addToAL g t . M.insert n xs $ h)
      Just _  -> (False, g)
  where
    h = maybe M.empty (M.insert n xs) (lookup t g)

-- Return all arrows for the given object.
-- E.g., arrows struct "employee" => ["employee", "department", "@", "@"]
arrows :: Schema -> Text -> [Text]
arrows s t = snd <$> fromMaybe [] (lookup t s)

-- ////////////////////////////////////////////////////////////////////////////
-- Query
-- ////////////////////////////////////////////////////////////////////////////

queryGraph :: Graph             -- ^ Haystack
           -> [(Text, [Int])]   -- ^ Query
           -> Graph
queryGraph hs = map $ \(t, xs) -> (t, M.fromList $ mapMaybe (f t) xs)
  where
    f :: Text -> Int -> Maybe (Int, [Int])
    f t n = lookup' hs t n >>= \xs -> Just (n, xs)
    lookup' :: Graph -> Text -> Int -> Maybe [Int]
    lookup' g t n = lookup t g >>= M.lookup n

exQuery :: Graph -> [(Text, [Int])] -> Schema -> Graph
exQuery hs = flip merge hs . queryGraph hs

