{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module Hippo.JS
  ( compile
  ) where

import Hippo.Core
import Control.Applicative
import Data.Maybe                ( fromMaybe )
import Data.Monoid
import Data.Text                 ( Text )

import qualified Data.Text       as T
import qualified Text.Show.Text  as T

-- | Introduce a container object for the item.
--     e.g., var resource = {};
container :: Text -> Text
container item = "var " <> item <> "={};"

-- | Define a trivial object which assigns the identity map to a property.
--     e.g., resource[1] = {id: 1};
identity :: Text -> Int -> Text
identity item id = item <> "[" <> key <> "]={id:" <> key <> "};"
  where key = T.show id

-- | Set up a relation between the source and target objects.
--     e.g., employee[3].worksIn = department[1];
relation :: (Text, Int) -> Text -> (Text, Int) -> Text
relation (source, s) arr (target, t) =
    source <> "["  <> T.show s
           <> "]." <> arr
           <> "="  <> target
           <> "["  <> T.show t <> "];"

-- | Set up a relation between the source and a stored item.
--     e.g., employee[3].firstName = 'Bob';
resource :: DataStore -> (Text, Int) -> Text -> Int -> Text
resource ds (source, s) arr index =
      source <> "["  <> T.show s
             <> "]." <> arr
             <> "="  <> item (dsLookup index ds) <> ";"
  where
    item = \case
      Just ( Str     s     ) -> "'" <> s <> "'"
      Just ( Zahl    z     ) -> T.show z
      Just ( Boolean False ) -> "false"
      Just ( Boolean True  ) -> "true"
      Just ( Real    r     ) -> T.show r
      Nothing                -> "null"

-- | Build return object from a list of objects.
--     e.g., var obj = {foo: foo, baz: baz}; return obj;
wrapper :: [Text] -> Text
wrapper xs  = "var obj={" <> T.intercalate "," xs' <> "};return obj"
  where xs' = map (\x -> x <> ":" <> x) xs

-- | Wrap the supplied code block in a function closure.
closure :: Text -> Text
closure js = "(function(){" <> js <> "}());"

-- | Generate boilerplate JS.
generate :: DataStore -> Graph -> Schema -> Text
generate ds grph struct = T.concat $ concatMap (flip concatMap $ keys grph)
    [ return . container
    -- ^ Empty initial object, e.g., var resource = {};
    , map <$> identity <*> (idmap grph)
    -- ^ Insert 'id' property, e.g., resource[1] = {id: 1};
    , rels ds grph struct ]
    -- ^ Establish relations,  e.g., department[1].manager = employee[2];

rels :: DataStore -> Graph -> Schema -> Text -> [Text]
rels ds grph struct itm = dictFold f (item grph itm)
  where
    f :: Int -> [Int] -> Text
    f m xs = T.concat $ zipWith (g m) xs (arrows itm struct)
    g :: Int -> Int -> (Text, Text) -> Text
    g m n (a, "@") = resource ds (itm, m) a n
    g m n (a,  b ) = relation (itm, m) a (b, n)

idmap :: Graph -> Text -> [Int]
idmap g = dictKeys . item g

arrows :: Text -> Schema -> [(Text, Text)]
arrows t s = fromMaybe [] (lookup t s)

item :: Graph -> Text -> Dict
item g o = fromMaybe emptyDict (lookup o g)

-- | Generate JavaScript closure from the provided Graph and Schema objects.
compile :: DataStore -> Graph -> Schema -> Text
compile ds g s = closure $ generate ds g s <> wrapper (keys g)

