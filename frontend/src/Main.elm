module Main exposing (..)

import Html exposing (Html, a, text, div, img, button, text, program, p, thead, th, td, tr,table, tbody)
import Html.Attributes exposing (src, target, href)
import Html.Events exposing (onClick)
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

type Msg =  Data (Result Http.Error (List Player)) | IncPage | DecPage | Clear | QUERY QueryState  

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

type alias Model = { players : List Player,
                     pageCount: Int ,
                     query: Maybe String,
                     page: Int }

initialModel : Model 
initialModel = { players = [], pageCount = 10 , query = Nothing, page = 0 }

init: (Model, Cmd Msg)
init =  (initialModel, initialModel |> constructUrl |> getPlayers)

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
view model = div [] 
          [ button [onClick IncPage] [text "forward"],
            button [onClick DecPage] [text "backward"],
            a [href (model |> constructUrl |> csvUrl), target "_blank" ] [text "View Download"],
          table [] [ thead []
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
        , tbody [] (List.map toTableRow model.players)
        ]]


constructUrl : Model -> String
constructUrl model = String.concat [url, "?limit=", (toString model.pageCount), "&page=", (toString model.page)]

csvUrl : String -> String 
csvUrl x = x ++ "&csv=chimichanga"

getPlayers : String ->  Cmd Msg
getPlayers url =
  let a = Http.get url (list playerDecoder) 
  in Http.send Data a 


update : Msg -> Model ->(Model, Cmd Msg)
update x model =  case x of Data (Ok players) -> ({ model | players = players }, Cmd.none)
                            Data (Err _) -> (model, Cmd.none)
                            IncPage -> let nmodel =  {model | page = model.page + 1}
                                       in (nmodel, nmodel |> constructUrl |> getPlayers )
                            DecPage -> let nmodel =  {model | page = model.page - 1}
                                       in (nmodel, nmodel |> constructUrl |> getPlayers )
                            Clear -> let nmodel =  {model | query = Nothing, pageCount = 10, page = 0}
                                       in (nmodel, nmodel |> constructUrl |> getPlayers )
                            TD_ASC -> let nmodel =  {model | query = Just "sort=td&direction=asc"}
                                       in (nmodel, nmodel |> constructUrl |> getPlayers )
                            QUERY x -> case x of TD_DESC ->
                                                 YDS_ASC -> 
                                                 YDS_DESC -> 
                                                 LNG_ASC -> 
                                                 LNG_DESC -> 
                                                 NAME String                            




subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

