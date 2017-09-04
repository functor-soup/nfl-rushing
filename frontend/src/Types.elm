module Types exposing (..)

import Json.Decode exposing (int, string, float, Decoder, list)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)

type QueryState = TD_ASC | TD_DESC | YDS_ASC | YDS_DESC | LNG_ASC | LNG_DESC | NAME String

type alias Player = {
    name : String
  , team : String
  , pos : String
  , att : Float
  , attG : Float
  , yds : Float
  , avg : Float
  , ydsG : Float
  , td : Float
  , lng : Float
  , fst : Float
  , fstPercent : Float
  , twentyPlus : Float
  , fortyPlus : Float
  , fumble : Float
}

type alias ApiResponse = {
   players : List Player,
   page : Int,
   totalPages: Int
}

playerDecoder : Decoder Player
playerDecoder =
  decode Player
    |> required "Player" string
    |> required "Team" string
    |> required "Pos" string
    |> required "Att" float
    |> required "Att/G" float
    |> required "Yds" float
    |> required "Avg" float
    |> required "Yds/G" float
    |> required "TD" float
    |> required "Lng"float
    |> required "1st" float
    |> required "1st%" float
    |> required "20+" float
    |> required  "40+" float
    |> required "FUM" float

apiResponseDecoder : Decoder ApiResponse
apiResponseDecoder = decode ApiResponse
    |> required "payload" (list playerDecoder)
    |> required "currentPage" int
    |> required "totalPages" int


