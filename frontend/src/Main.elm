module Main exposing (..)

import Html exposing (Html, a, text, div, img, button, text, program, p, thead, th, td, tr,table, tbody, input)
import Html.Attributes exposing (src, target, href, hidden)
import Html.Events exposing (onClick, onInput)
import Http
import Types exposing (..)
import List exposing (all)

url : String
url = "http://localhost:8000/players"

main =
  Html.program
    { init = init 
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg =  Data (Result Http.Error ApiResponse) | IncPage | DecPage | Clear | QUERY QueryState 

type alias Model = { players : List Player,
                     pageCount: Int,
                     query: String, 
                     page: Int,
                     totalPages : Int}

initialModel : Model 
initialModel = { players = [], pageCount = 10 , query = "", page = 0, totalPages = 0 }

init: (Model, Cmd Msg)
init =  (initialModel, initialModel |> constructUrl |> getPlayers)


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

view : Model -> Html Msg
view model = let bfv = hideForwardButton model
                 bbv = hideBackwardButton model
             in div [] 
             [ 
             input [onInput (\x -> QUERY (NAME x))] [],
             button [onClick IncPage, hidden bfv] [text "forward"], 
             button [onClick DecPage, hidden bbv] [text "backward"], 
             a [href (model |> constructUrl |> csvUrl), target "_blank" ] [text "Page CSV Download"], 
             table [] [ thead [] 
             [ th [][text "Player"]
            , th [][text "Team"]
            , th [][text "Pos"]
            , th [][text "Att"]
            , th [][text "Att/G"]
            , th [][text "Yds" , button [onClick (QUERY YDS_ASC)][text "up"], button [onClick (QUERY YDS_DESC)][text "down"]]
            , th [][text "Avg"]
            , th [][text "Yds/G"]
            , th [][text "TD",button [onClick (QUERY TD_ASC)][text "up"], button [onClick (QUERY TD_DESC)][text "down"]]
            , th [][text "Lng",button [onClick (QUERY LNG_ASC)][text "up"], button [onClick (QUERY LNG_DESC)][text "down"]]
            , th [][text "1st"]
            , th [][text "1st%"]
            , th [][text "20+"]
            , th [][text "40+"]
            , th [][text "FUM"]
            ]
        , tbody [] (List.map toTableRow model.players)
        ]
        , button [onClick Clear] [text "Clear"]
        , a [href (model |> constructUrlWithoutPageLimit |> csvUrl), target "_blank" ] [text "Full CSV Download"]
        ]


constructUrl : Model -> String
constructUrl model = String.concat [url, "?limit=", (toString model.pageCount), "&page=", (toString model.page), (model.query)]

constructUrlWithoutPageLimit : Model -> String
constructUrlWithoutPageLimit model = String.concat [url,(model.query)]

csvUrl : String -> String 
csvUrl x = x ++ "&csv=chimichanga"

getPlayers : String ->  Cmd Msg
getPlayers url =
  let a = Http.get url apiResponseDecoder
  in Http.send Data a 


update : Msg -> Model ->(Model, Cmd Msg)
update x model =  case x of Data (Ok response) -> ({ model | players = response.players , page = response.page, totalPages = response.totalPages}, Cmd.none)
                            Data (Err _) -> (model, Cmd.none)
                            IncPage -> let nmodel =  {model | page = model.page + 1}
                                       in (model, nmodel |> constructUrl |> getPlayers )
                            DecPage -> let nmodel =  {model | page = model.page - 1}
                                       in (model, nmodel |> constructUrl |> getPlayers )
                            Clear -> let nmodel =  {model | query = "", pageCount = 10, page = 0}
                                       in (nmodel, nmodel |> constructUrl |> getPlayers )
                            QUERY x -> case x of TD_DESC -> let nmodel =  {model | query = "&sort=td&direction=desc"} 
                                                            in (nmodel, nmodel |> constructUrl |> getPlayers )
                                                 TD_ASC -> let nmodel =  {model | query = "&sort=td&direction=asc"} 
                                                           in (nmodel, nmodel |> constructUrl |> getPlayers )
                                                 YDS_ASC -> let nmodel =  {model | query = "&sort=yds&direction=asc"} 
                                                            in (nmodel, nmodel |> constructUrl |> getPlayers )
                                                 YDS_DESC -> let nmodel =  {model | query = "&yds=td&direction=desc"} 
                                                             in (nmodel, nmodel |> constructUrl |> getPlayers )
                                                 LNG_ASC ->  let nmodel =  {model | query = "&sort=lng&direction=asc"} 
                                                             in (nmodel, nmodel |> constructUrl |> getPlayers )
                                                 LNG_DESC -> let nmodel =  {model | query = "&sort=lng&direction=desc"} 
                                                             in (nmodel, nmodel |> constructUrl |> getPlayers )
                                                 NAME y -> let nmodel =  {model | query = "&name=" ++ y} 
                                                           in (nmodel, nmodel |> constructUrl |> getPlayers )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

hideForwardButton : Model -> Bool
hideForwardButton model = all (\x -> x == True) [model.totalPages-1  <=  model.page]

hideBackwardButton : Model -> Bool
hideBackwardButton model = all (\x -> x == True) [model.page == 0 ]

hidePartialButton : Model -> Bool
hidePartialButton = hideForwardButton

