{-# LANGUAGE OverloadedStrings #-}

module Query
  ( stringedParams
  , layerTraversal
  , layer
  ) where

import Data.List (intersect, sortBy)
import Data.Map.Strict
       (Map(..), (!), filterWithKey, fromList, keys, lookup)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Text.Lazy (Text, toLower, isInfixOf)
import Prelude hiding (lookup)
import Types (CleanedPlayer(..))

-- ================================= Query String List ============================================
stringedParams :: [Text]
stringedParams = ["name", "sort", "direction"]

type Layer a = Map Text ([CleanedPlayer] -> a -> [CleanedPlayer])

layer :: Layer Text
layer =
  fromList
    [ ("sort", \x y -> maybe x (\a -> sortBy a x) (lookup y sortify))
    , ("direction", flip orderify)
    , ("name", \x y -> filter (isInfixOf (toLower y) . toLower . name_) x)
    ]

-- responsible for laying out the order of the list to be sent back
-- i.e ascending or descending
orderify :: Text -> [CleanedPlayer] -> [CleanedPlayer]
orderify x y =
  case x of
    "desc" -> reverse y
    _ -> y

sortify :: Map Text (CleanedPlayer -> CleanedPlayer -> Ordering)
sortify =
  fromList
    [("lng", comparing lng_), ("td", comparing td_), ("yds", comparing yds_)]

-- keep note the intersection is to maintain the order, the query has to coerced into
-- an order to make sense
layerTraversal ::
     Map Text a -> Layer a -> [Text] -> [CleanedPlayer] -> [CleanedPlayer]
layerTraversal w x y z =
  let i = intersect y (keys w)
  in foldl (\acc k -> maybe acc (\g -> g acc (w ! k)) (lookup k x)) z i
