{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Player(..)
  , CleanedPlayer(..)
  , ApiResponse(..)
  , csvHeader
  ) where

import Control.Applicative
import Data.Aeson
import qualified Data.Csv as C
import Data.Text.Lazy
import GHC.Generics

data Player = Player
  { _name :: Text
  , _team :: Text
  , _pos :: Text
  , _att :: Float
  , _attG :: Float
  , _yds :: Either Text Float
  , _avg :: Float
  , _ydsG :: Float
  , _td :: Float
  , _lng :: Either Text Float
  , _fst :: Float
  , _fstPercent :: Float
  , _twentyPlus :: Float
  , _fortyPlus :: Float
  , _fumble :: Float
  } deriving (Show)

instance FromJSON Player where
  parseJSON =
    withObject "Player" $ \v ->
      Player <$> v .: "Player" <*> v .: "Team" <*> v .: "Pos" <*> v .: "Att" <*>
      v .: "Att/G" <*>
      ((Left <$> v .: "Yds") <|> (Right <$> v .: "Yds")) <*>
      v .: "Avg" <*>
      v .: "Yds/G" <*>
      v .: "TD" <*>
      ((Left <$> v .: "Lng") <|> (Right <$> v .: "Lng")) <*>
      v .: "1st" <*>
      v .: "1st%" <*>
      v .: "20+" <*>
      v .: "40+" <*>
      v .: "FUM"

data CleanedPlayer = CPlayer
  { name_ :: Text
  , team_ :: Text
  , pos_ :: Text
  , att_ :: Float
  , attG_ :: Float
  , yds_ :: Float
  , avg_ :: Float
  , ydsG_ :: Float
  , td_ :: Float
  , lng_ :: Float
  , fst_ :: Float
  , fstPercent_ :: Float
  , twentyPlus_ :: Float
  , fortyPlus_ :: Float
  , fumble_ :: Float
  } deriving (Show, Generic)

-- instance for writing to Json
instance ToJSON CleanedPlayer where
  toJSON (CPlayer name team pos att attG yds avg ydsG td lng fst fstPercent twentyP fortyP fumble) =
    object
      [ "Player" .= name
      , "Team" .= team
      , "Pos" .= pos
      , "Att" .= att
      , "Att/G" .= attG
      , "Yds" .= yds
      , "Avg" .= avg
      , "Yds/G" .= ydsG
      , "TD" .= td
      , "Lng" .= lng
      , "1st" .= fst
      , "1st%" .= fstPercent
      , "20+" .= twentyP
      , "40+" .= fortyP
      , "FUM" .= fumble
      ]

-- instance for writing to CSV
instance C.ToNamedRecord CleanedPlayer where
  toNamedRecord (CPlayer name team pos att attG yds avg ydsG td lng fst fstPercent twentyP fortyP fumble) =
    C.namedRecord
      [ "Player" C..= name
      , "Team" C..= team
      , "Pos" C..= pos
      , "Att" C..= att
      , "Att/G" C..= attG
      , "Yds" C..= yds
      , "Avg" C..= avg
      , "Yds/G" C..= ydsG
      , "TD" C..= td
      , "Lng" C..= lng
      , "1st" C..= fst
      , "1st%" C..= fstPercent
      , "20+" C..= twentyP
      , "40+" C..= fortyP
      , "FUM" C..= fumble
      ]

csvHeader :: C.Header
csvHeader =
  C.header
    [ "Player"
    , "Team"
    , "Pos"
    , "Att"
    , "Att/G"
    , "Yds"
    , "Avg"
    , "Yds/G"
    , "TD"
    , "Lng"
    , "1st"
    , "1st%"
    , "20+"
    , "40+"
    , "FUM"
    ]

data ApiResponse = ApiResponse {
                               payload :: [CleanedPlayer] ,
                               currentPage :: Int,
                               remainPages :: Int
                               } deriving (Show, Generic)

instance ToJSON ApiResponse
