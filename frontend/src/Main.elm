module Main exposing (..)

import Html exposing (Html, text, div, img, button, text, program, p, thead, th, td, tr,table, tbody)
import Html.Attributes exposing (src)
import Json.Decode exposing (int, string, float, Decoder, list)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Keyboard
import Http
import Task exposing (..)

main =
  Html.program
    { init = init 
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg =  Data (Result Http.Error Model)

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

type alias Model = List Player

init: (Model, Cmd Msg)
init =  ([], getPlayers)

url : String
url = "http://localhost:8000/players"


toTableRow: Player -> Html Msg
toTableRow x =
  tr []
     [
     td[][text x.name],
     td[][text x.team],
     td[][text x.pos],
     td[][text (toString x.att)],
     td[][text (toString x.attG)],
     td[][text (toString x.yds)],
     td[][text (toString x.avg)],
     td[][text (toString x.ydsG)],
     td[][text (toString x.td)],
     td[][text (toString x.lng)],
     td[][text (toString x.fst)],
     td[][text (toString x.fstPercent)],
     td[][text (toString x.twentyPlus)],
     td[][text (toString x.fortyPlus)],
     td[][text (toString x.fumble)]
     ]


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

view : Model -> Html Msg
view model = table []
        [ thead []
            [ th [][text "Player"]
            , th [][text "Team"]
            , th [][text "Pos"]
            , th [][text "Att"]
            , th [][text "Att/G"]
            , th [][text "Yds"]
            , th [][text "Avg"]
            , th [][text "Yds/G"]
            , th [][text "TD"]
            , th [][text "Lng"]
            , th [][text "1st"]
            , th [][text "1st%"]
            , th [][text "20+"]
            , th [][text "40+"]
            , th [][text "FUM"]
            ]
        , tbody [] (List.map toTableRow model)
        ]

getPlayers : Cmd Msg
getPlayers =
  let a = Http.get url (list playerDecoder) 
  in Http.send Data a 

update : Msg -> Model ->(Model, Cmd Msg)
update x model =  case x of Data (Ok players) -> (players, Cmd.none)
                            Data (Err _) -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

