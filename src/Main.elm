module Main exposing (..)

import Browser
import Html exposing (Html, text, div, p)
import Html.Attributes exposing (class)


-- MAIN

main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL

type alias Row = List Int
type alias Board = List Row
type alias Model = Board

init : () -> ( Model, Cmd Msg )
init _ =
    ( [ [2,0,0,2]
      , [0,0,0,0]
      , [0,0,0,0]
      , [0,2,4,8]
      ]
    , Cmd.none
    )


-- UPDATE

type Msg = Up | Right | Down | Left

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }

view : Model -> Document Msg
view model =
    { title = "2048"
    , body = [renderBoard model]
    }

renderBoard : Board -> Html Msg
renderBoard board =
    div [ class "Board" ]
        ( List.map renderRow board )

renderRow : Row -> Html Msg
renderRow row =
    div [ class "Row" ]
        (List.map renderTile row)

renderTile : Int -> Html Msg
renderTile n =
    div [ class (String.concat ["Tile Tile-", String.fromInt n]) ]
        [ p []
            [ if n == 0
                then text ""
                else text (String.fromInt n)
                ] ]

