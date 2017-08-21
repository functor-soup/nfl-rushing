module Main where

import Conversion
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Exit
import Lib
import Types
import Server

main :: IO ()
main = do
  bb <- B.readFile "rushing.json"
  let a = eitherDecode bb :: Either String [Player]
  either die (server . map conversion) a
