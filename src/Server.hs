{-# LANGUAGE OverloadedStrings #-}

module Server
  ( server
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Csv as C
import Data.Map.Strict (filterWithKey, fromList, lookup)
import Data.Text.Lazy (Text)
import Prelude hiding (lookup)
import Query (layer, layerTraversal, stringedParams)
import Types (CleanedPlayer(..), csvHeader)
import Web.Scotty
import Data.Monoid ((<>))

fileName :: Text
fileName = "Players.csv"

-- ============================== Routes ==============================================

heartBeatRoute :: ActionM ()
heartBeatRoute = text "Monkeys managed to type out Shakespear's works"

search :: [CleanedPlayer] -> ActionM ()
search x = do
  p <- fmap fromList params
  -- get all the valid string params
  let sp = filterWithKey (\x _ -> elem x stringedParams) p
      results = layerTraversal sp layer stringedParams x
  liftIO $ putStrLn . show $ p -- should put this in a writer monad transformer
  maybe
    (json results)
    (\_ -> do
       setHeader "Content-type" "text/csv"
       setHeader "Content-disposition" $ "attachment;filename=" <> fileName
       raw $ C.encodeByName csvHeader results)
    (lookup "csv" p)

-- ========================== main server export ========================================

routes :: [CleanedPlayer] -> ScottyM ()
routes x = get "/alive" heartBeatRoute >> get "/players" (search x)


server :: [CleanedPlayer] -> Int ->  IO ()
server x port = scotty port (routes x)
