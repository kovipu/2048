module Main exposing (..)

import Browser
import Html exposing (Html, text, div, p)
import Html.Attributes exposing (class)
import Html.Events exposing (on, onMouseUp)
import Debug exposing (log)
import Json.Decode exposing (Decoder, map4, at, float, int)
import List.Extra exposing (transpose)


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
type alias Model =
    { board : Board
    , isMouseDown : Bool
    , originalCoordinates : (Int, Int)
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = [ [0,0,2,2]
                , [0,0,0,0]
                , [0,0,0,0]
                , [0,0,8,4]
                ]
      , isMouseDown = False
      , originalCoordinates = (0, 0)
      }
    , Cmd.none
    )



-- UPDATE

type alias MouseMoveData =
    { offsetX : Int
    , offsetY : Int
    , offsetHeight : Float
    , offsetWidth : Float
    }

type Direction = Up | Right | Down | Left | None
type Msg = MouseDown MouseMoveData | MouseMove MouseMoveData | MouseUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown data ->
            ({ board = model.board
                , isMouseDown = True
                , originalCoordinates = (data.offsetX, data.offsetY)
                }, Cmd.none)

        MouseMove data ->
            let
                direction = findMoveDirection model.originalCoordinates (data.offsetX, data.offsetY)
            in
                if direction == None || not model.isMouseDown
                then (model, Cmd.none)
                else ({ board = model.board
                          |> normalize direction
                          |> moveTiles
                          |> deNormalize direction
                      , isMouseDown = False
                      , originalCoordinates = model.originalCoordinates
                      }, Cmd.none)

        MouseUp ->
            ({ board = model.board
             , isMouseDown = False
             , originalCoordinates = (0, 0)
             }, Cmd.none)

findMoveDirection : (Int, Int) -> (Int, Int) -> Direction
findMoveDirection (originalX, originalY) (newX, newY) =
    let
        offsetX = originalX - newX
        offsetY = originalY - newY
        threshold = 50
    in
        if (abs offsetX) > threshold
        then
            if offsetX > 0
            then Left
            else Right
        else if abs offsetY > threshold
        then
            if offsetY > 0
            then Up
            else Down
        else None

normalize : Direction -> Board -> Board
normalize direction board =
    case direction of
        Up ->
            board
                |> transpose
                |> List.reverse

        Right ->
            board
                |> List.map List.reverse

        Down ->
            board
                |> List.reverse
                |> transpose

        Left ->
            board

        _ ->
            board

-- assumes the board is normalized so we only need to handle moving left
moveTiles : Board -> Board
moveTiles board =
    List.map moveRow board


moveRow : Row -> Row
moveRow row =
    row
        |> List.foldr stepFunction []
        |> padWithZeros
        |> List.take 4


stepFunction : Int -> List Int -> List Int
stepFunction n acc =
    let h = List.head acc
    in
        if h == Nothing
            then [n]
        else if n == 0
            then acc
        else if h == Just n
            then 2*n :: List.drop 1 acc
        else
            n :: acc

-- pad a list with zeroes
padWithZeros : List Int -> List Int
padWithZeros row = row ++ [0,0,0,0]


deNormalize : Direction -> Board -> Board
deNormalize direction board =
    case direction of
        Up ->
            board
                |> List.reverse
                |> transpose

        Right ->
            board
                |> List.map List.reverse

        Down ->
            board
                |> transpose
                |> List.reverse

        Left ->
            board

        _ ->
            board


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
    , body = [renderBoard model.board]
    }

renderBoard : Board -> Html Msg
renderBoard board =
    div [ class "BoardContainer" ]
        [ div [ class "Board" ]
              ( List.map renderRow board )
         , div [ class "TouchListener"
               , on "mousedown" (Json.Decode.map MouseDown decodeMouseData)
               , on "mousemove" (Json.Decode.map MouseMove decodeMouseData)
               , onMouseUp MouseUp
               ]
               []
         ]

decodeMouseData : Decoder MouseMoveData
decodeMouseData =
    map4 MouseMoveData
        (at [ "offsetX" ] int)
        (at [ "offsetY" ] int)
        (at [ "target", "offsetHeight" ] float)
        (at [ "target", "offsetWidth" ] float)

renderRow : Row -> Html Msg
renderRow row =
    div [ class "Row" ]
        (List.map renderTile row)

renderTile : Int -> Html Msg
renderTile n =
    let
        num = String.fromInt n
    in
        div [ class (String.concat ["Tile Tile-", num]) ]
            [ p []
                [ if n == 0
                    then text ""
                    else text num
                    ] ]

