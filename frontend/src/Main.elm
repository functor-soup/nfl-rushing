module Main exposing (..)

import Html exposing (Html, a, text, div, img, button, text, program, p, thead, th, td, tr,table, tbody, input)
import Html.Attributes exposing (src, target, href, hidden, class, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Types exposing (..)
import List exposing (all)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Table as T
import Bootstrap.Button as B
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Form.Input as Input


upSymbol : String
upSymbol = "↑" 

downSymbol : String
downSymbol = "↓"

url : String
url = "/players"

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


toTableRow: Player -> T.Row Msg
toTableRow x =
  T.tr []
     [
     T.td[][text x.name],
     T.td[][text x.team],
     T.td[][text x.pos],
     T.td[][text (toString x.att)],
     T.td[][text (toString x.attG)],
     T.td[][text (toString x.yds)],
     T.td[][text (toString x.avg)],
     T.td[][text (toString x.ydsG)],
     T.td[][text (toString x.td)],
     T.td[][text (toString x.lng)],
     T.td[][text (toString x.fst)],
     T.td[][text (toString x.fstPercent)],
     T.td[][text (toString x.twentyPlus)],
     T.td[][text (toString x.fortyPlus)],
     T.td[][text (toString x.fumble)]
     ]

view : Model -> Html Msg
view model = let bfv = hideForwardButton model
                 bbv = hideBackwardButton model
             in  Grid.container [] 
             [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS,
             , Grid.row [] [ Grid.col [] [Input.text [Input.attrs [onInput (\x -> QUERY (NAME x)), placeholder "Search by Player name"]]] ]
             , Grid.row [] [ Grid.col [] [ B.button [ B.primary, B.attrs [onClick IncPage, hidden bfv, class "float-right"]] [text "next page"]
                                         , B.button [ B.primary, B.attrs [onClick DecPage, hidden bbv, class "float-left"]] [text "prev page"]]]
             , Grid.row [Row.centerXs] [ Grid.col [Col.xs4] [ B.linkButton [B.primary, B.attrs [href (model |> constructUrl |> csvUrl), target "_blank"]] [text "Page CSV Download"]]]
             , Grid.row [] 
             [ Grid.col []
                [ 
                  T.table 
                  { options = [ T.striped], 
                   thead = T.simpleThead [ T.th [][text "Player"] 
                   , T.th [][text "Team"] 
                   , T.th [][text "Pos"] 
                   , T.th [][text "Att"] 
                   , T.th [][text "Att/G"] 
                   , T.th [][text "Yds" , button [onClick (QUERY YDS_ASC)][text upSymbol], button [onClick (QUERY YDS_DESC)][text downSymbol]] 
                   , T.th [][text "Avg"] 
                   , T.th [][text "Yds/G"] 
                   , T.th [][text "TD",button [onClick (QUERY TD_ASC)][text upSymbol], button [onClick (QUERY TD_DESC)][text downSymbol]] 
                   , T.th [][text "Lng",button [onClick (QUERY LNG_ASC)][text upSymbol], button [onClick (QUERY LNG_DESC)][text downSymbol]] 
                   , T.th [][text "1st"] 
                   , T.th [][text "1st%"] 
                   , T.th [][text "20+"] 
                   , T.th [][text "40+"] 
                   , T.th [][text "FUM"] 
                   ], 
                   tbody = T.tbody [] (List.map toTableRow model.players) 
                   } 
                   , B.button [B.secondary , B.attrs [onClick Clear, class "float-right"]] [text "Clear"] 
                   , a [href (model |> constructUrlWithoutPageLimit |> csvUrl), target "_blank" ] [text "Full CSV Download"] 
                ] ] 
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
                                                 YDS_DESC -> let nmodel =  {model | query = "&sort=yds&direction=desc"} 
                                                             in (nmodel, nmodel |> constructUrl |> getPlayers )
                                                 LNG_ASC ->  let nmodel =  {model | query = "&sort=lng&direction=asc"} 
                                                             in (nmodel, nmodel |> constructUrl |> getPlayers )
                                                 LNG_DESC -> let nmodel =  {model | query = "&sort=lng&direction=desc"} 
                                                             in (nmodel, nmodel |> constructUrl |> getPlayers )
                                                 NAME y -> let nmodel =  {model | query = "&name=" ++ y, page = 0 } 
                                                           in (nmodel, nmodel |> constructUrl |> getPlayers )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

hideForwardButton : Model -> Bool
hideForwardButton model = all (\x -> x == True) [model.totalPages-1  <=  model.page]

hideBackwardButton : Model -> Bool
hideBackwardButton model = all (\x -> x == True) [model.page == 0 ]

hidePartialButton : Model -> Bool
hidePartialButton = hideForwardButton

