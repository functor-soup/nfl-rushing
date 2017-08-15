module Main where

import Conversion
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Lib
import Types

main :: IO ()
main = do
  bb <- B.readFile "rushing.json"
  let a = eitherDecode bb :: Either String [Player]
  putStrLn . show . fmap (map _yds) $ a
  putStrLn . show . fmap (map $ yds_ . conversion) $ a
