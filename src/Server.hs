{-# LANGUAGE OverloadedStrings #-}

module Server
  ( server
  ) where

import Data.Function ((&))
import Data.List (sortBy, intersect)
import Data.Map.Strict (Map(..), fromList, lookup, keys, filterWithKey, (!))
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Text.Lazy (Text, toLower)
import Prelude hiding (lookup)
import Types (CleanedPlayer(..))
import Web.Scotty
import Control.Monad.IO.Class (MonadIO, liftIO)
--import Data.List.Split (chunksOf)

port :: Int
port = 8000

-- ============================== Routes ==============================================
heartBeatRoute :: ActionM ()
heartBeatRoute = text "Monkeys managed to type out Shakespear's works"

search :: [CleanedPlayer] -> ActionM ()
search x
  -- get all the float ones
 = do
  p <- fmap fromList params
  let sp = filterWithKey (\x _ -> elem x stringedParams) p
      results = layerTraversal sp layer2 stringedParams x
  liftIO $ putStrLn . show  $ sp
  json results

routes :: [CleanedPlayer] -> ScottyM ()
routes x = get "/alive" heartBeatRoute >> get "/players" (search x)

server :: [CleanedPlayer] -> IO ()
server x = scotty port (routes x)

-- ================================= Query String List ============================================

--floatParams :: [Text]
--floatParams = keys layer1

stringedParams :: [Text]
stringedParams = ["name","sort","direction"]

type Layer a = Map Text ([CleanedPlayer] -> a -> [CleanedPlayer])

--layer1 :: Layer Float
--layer1 =
--  fromList
--    [ ("limit", \x y -> chunks y x)
--     ,("page", \x y -> chunksOf y x)
--    ]

layer2 :: Layer Text
layer2 =
  fromList
    [ ("sort", \x y -> maybe x (\a -> sortBy a x) (lookup y sortify2))
    , ("direction", flip sortify)
    , ("name", \x y -> filter ((==) (toLower y) . toLower. name_) x)
    ]

sortify :: Text -> [CleanedPlayer] -> [CleanedPlayer]
sortify x y =
  case x of
    "desc" -> reverse y
    _ -> y

sortify2 :: Map Text (CleanedPlayer -> CleanedPlayer -> Ordering)
sortify2 =
  fromList
    [("lng", comparing lng_), ("td", comparing td_), ("yds", comparing yds_)]

-- keep note the intersection is to maintain the order, the query has to coerced into
-- an order to make sense
layerTraversal :: Map Text a -> Layer a -> [Text] -> [CleanedPlayer] -> [CleanedPlayer]
layerTraversal w x y z =
  let i = intersect y (keys w) in
  foldl (\acc k -> maybe acc (\g -> g acc (w ! k)) (lookup k x)) z i
