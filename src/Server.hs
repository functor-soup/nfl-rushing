{-# LANGUAGE OverloadedStrings #-}

module Server
  ( server
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Csv as C
import Data.Function ((&))
import Data.Map.Strict (filterWithKey, fromList, lookup)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text, unpack)
import Prelude hiding (lookup)
import Query (layer, layerTraversal, stringedParams)
import Text.Read (readMaybe)
import Types (CleanedPlayer(..), ApiResponse(..), csvHeader)
import Utils (group)
import Web.Scotty
import Network.Wai.Middleware.Static

fileName :: Text
fileName = "Players.csv"

staticDirectory :: String
staticDirectory = "./frontend/build"

-- ============================== Routes ==============================================
heartBeatRoute :: ActionM ()
heartBeatRoute = text "Monkeys managed to type out Shakespear's works"

search :: [CleanedPlayer] -> ActionM ()
search x = do
  p <- fmap fromList params
  liftIO $ putStrLn . show $ p -- should put this in a writer monad transformer
  -- get all the valid string params
  let sp = filterWithKey (\x _ -> elem x stringedParams) p
      results = layerTraversal sp layer stringedParams x
      paginParams = mapM (`lookup` p) ["limit", "page"]
      presults =
        paginParams >>=
        mapM (fmap abs . readMaybe . unpack) &
        maybe
          (ApiResponse results 0 0)
          (\[x, y] -> let groups = group x $ results
                          lgroups = length groups
                          r = concat . take 1 . drop y $ groups
                       in ApiResponse r y  lgroups)
  maybe
    (json presults)
    (\_ -> do
       setHeader "Content-type" "text/csv"
       setHeader "Content-disposition" $ "attachment;filename=" <> fileName
       raw $ C.encodeByName csvHeader $ payload presults)
    (lookup "csv" p)



-- ========================== main server export ========================================
routes :: [CleanedPlayer] -> ScottyM ()
routes x = do
  middleware $ staticPolicy (noDots >-> addBase staticDirectory)
  get "/alive" heartBeatRoute >> get "/players" (search x) >> (get "/" $ file "./frontend/build/index.html")

server :: [CleanedPlayer] -> Int -> IO ()
server x port = scotty port (routes x)
