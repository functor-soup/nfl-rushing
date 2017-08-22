module Main where

import Conversion
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Exit
import Lib
import Types
import Server

port :: Int
port = 8000

main :: IO ()
main = do
  bb <- B.readFile "rushing.json"
  let a = eitherDecode bb :: Either String [Player]
  either die (flip server port . map conversion) a
