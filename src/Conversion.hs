{-# LANGUAGE OverloadedStrings #-}

module Conversion
  ( conversion
  ) where

import Data.Monoid
import Data.Text.Lazy
import Types

lngSep :: Text
lngSep = "T"

yardsSep :: Text
yardsSep = ","

cleanup :: Text -> Either Text Float -> Float
cleanup x = either (read . unpack . mconcat . splitOn x) id

cleanupLng :: Either Text Float -> Float
cleanupLng = cleanup lngSep

cleanupYards :: Either Text Float -> Float
cleanupYards = cleanup yardsSep

conversion :: Player -> CleanedPlayer
conversion (Player a b c d e f g h i j k l m n o) =
  let f_ = cleanupYards f
      j_ = cleanupLng j
  in CPlayer a b c d e f_ g h i j_ k l m n o


