module Main exposing (..)

import Browser
import Html exposing (Html, text, div)


main =
    Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Row = List Int
type alias Board = List Row
type alias Model = Board

init : Model
init =
    [ [2,0,0,0]
    , [0,0,0,0]
    , [0,0,0,0]
    , [0,0,0,0]
    ]


-- UPDATE

type Msg = Up | Right | Down | Left

update : Msg -> Model -> Model
update msg model =
    case msg of
        Up ->
            model

        Right ->
            model

        Down ->
            model

        Left ->
            model


-- VIEW

view : Model -> Html Msg
view model = renderBoard model

renderBoard : Board -> Html Msg
renderBoard board =
    div []
        ( List.map renderRow board )

renderRow : Row -> Html Msg
renderRow row =
    div []
        (List.map renderTile row)

renderTile : Int -> Html Msg
renderTile n =
    div []
        [ text (String.fromInt n) ]

