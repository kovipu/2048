module Main exposing (..)

import Array
import Browser
import Browser.Events exposing (onKeyDown)
import Debug exposing (log)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Html.Events.Extra.Touch as Touch
import Json.Decode as Json
import List.Extra exposing (transpose)
import Random



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Row =
    List Int


type alias Board =
    List Row


type alias Model =
    { board : Board
    , touchInProgress : Bool
    , originalTouchCoords : ( Float, Float )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board =
            [ [ 0, 2, 2, 2 ]
            , [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            ]
      , touchInProgress = False
      , originalTouchCoords = ( 0, 0 )
      }
    , Cmd.none
    )



-- UPDATE


type Direction
    = Up
    | Right
    | Down
    | Left
    | None


type Msg
    = TouchStart ( Float, Float )
    | TouchMove ( Float, Float )
    | TouchEnd ( Float, Float )
    | Move Direction
    | NewTile ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TouchStart data ->
            ( { model
                | touchInProgress = True
                , originalTouchCoords = data
              }
            , Cmd.none
            )

        TouchMove data ->
            let
                direction =
                    findMoveDirection model.originalTouchCoords data

                board =
                    model.board
                        |> normalize direction
                        |> moveTiles
                        |> deNormalize direction

                boardWithCoords =
                    addCoordinates board

                generateIndex =
                    Random.int 0 (List.length (filterZeros boardWithCoords) - 1)

                generateNewTileValue =
                    Random.weighted
                        ( 90, 2 )
                        [ ( 10, 4 ) ]
            in
            if direction == None || not model.touchInProgress then
                ( model, Cmd.none )

            else
                ( { model
                    | board = board
                    , touchInProgress = False
                  }
                , Random.generate NewTile (Random.pair generateIndex generateNewTileValue)
                )

        TouchEnd _ ->
            ( { model | touchInProgress = False }
            , Cmd.none
            )

        Move direction ->
            if direction == None then
                ( model, Cmd.none )

            else
                let
                    board =
                        model.board
                            |> normalize direction
                            |> moveTiles
                            |> deNormalize direction

                    boardWithCoords =
                        addCoordinates board

                    generateIndex =
                        Random.int 0 (List.length (filterZeros boardWithCoords) - 1)

                    generateNewTileValue =
                        Random.weighted
                            ( 90, 2 )
                            [ ( 10, 4 ) ]
                in
                ( { model | board = board }
                , Random.generate NewTile (Random.pair generateIndex generateNewTileValue)
                )

        NewTile ( rn, tile ) ->
            let
                ( randX, randY, _ ) =
                    addCoordinates model.board
                        |> filterZeros
                        |> Array.fromList
                        |> Array.get rn
                        |> Maybe.withDefault ( 0, 0, 0 )

                board =
                    List.indexedMap
                        (\y row ->
                            List.indexedMap
                                (\x n ->
                                    if x == randX && y == randY then
                                        tile

                                    else
                                        n
                                )
                                row
                        )
                        model.board
            in
            ( { model | board = board }, Cmd.none )


findMoveDirection : ( Float, Float ) -> ( Float, Float ) -> Direction
findMoveDirection ( originalX, originalY ) ( newX, newY ) =
    let
        offsetX =
            originalX - newX

        offsetY =
            originalY - newY

        threshold =
            50
    in
    if abs offsetX > threshold then
        if offsetX > 0 then
            Left

        else
            Right

    else if abs offsetY > threshold then
        if offsetY > 0 then
            Up

        else
            Down

    else
        None


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
        |> List.filter (\n -> n /= 0)
        |> slideRow
        |> padWithZeros
        |> List.take 4


slideRow : List Int -> List Int
slideRow row =
    let
        first =
            Maybe.withDefault -1 (List.head row)

        second =
            Maybe.withDefault -1 (List.head (List.drop 2 row))
    in
    -- less than 2 tiles -> nothing to join
    if List.length row < 2 then
        row
        -- first and second same value -> join them

    else if first == second then
        [ first + second ] ++ slideRow (List.drop 2 row)
        -- first and second not same value -> do not join

    else
        [ first ] ++ slideRow (List.drop 1 row)


stepFunction : Int -> List Int -> List Int
stepFunction n acc =
    let
        h =
            List.head acc
    in
    if h == Nothing then
        [ n ]

    else if n == 0 then
        acc

    else if h == Just n then
        2 * n :: List.drop 1 acc

    else
        n :: acc



-- pad a list with zeroes


padWithZeros : List Int -> List Int
padWithZeros row =
    row ++ [ 0, 0, 0, 0 ]


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


addCoordinates : Board -> List (List ( Int, Int, Int ))
addCoordinates board =
    List.indexedMap
        (\y row ->
            List.indexedMap (\x num -> ( x, y, num )) row
        )
        board


filterZeros : List (List ( Int, Int, Int )) -> List ( Int, Int, Int )
filterZeros board =
    List.concat board
        |> List.filter (\( _, _, n ) -> n == 0)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown keyDecoder


keyDecoder : Json.Decoder Msg
keyDecoder =
    Json.map toDirection (Json.field "key" Json.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowUp" ->
            Move Up

        "ArrowRight" ->
            Move Right

        "ArrowDown" ->
            Move Down

        "ArrowLeft" ->
            Move Left

        "w" ->
            Move Up

        "a" ->
            Move Left

        "s" ->
            Move Down

        "d" ->
            Move Right

        _ ->
            Move None



-- VIEW


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "2048"
    , body = [ renderBoard model.board ]
    }


renderBoard : Board -> Html Msg
renderBoard board =
    div [ class "BoardContainer" ]
        [ div [ class "Board" ]
            (List.map renderRow board)
        , div
            [ class "TouchListener"
            , Touch.onStart (TouchStart << touchCoordinates)
            , Touch.onMove (TouchMove << touchCoordinates)
            , Touch.onEnd (TouchEnd << touchCoordinates)
            ]
            []
        ]


renderRow : Row -> Html Msg
renderRow row =
    div [ class "Row" ]
        (List.map renderTile row)


renderTile : Int -> Html Msg
renderTile n =
    let
        num =
            String.fromInt n
    in
    div [ class (String.concat [ "Tile Tile-", num ]) ]
        [ p []
            [ if n == 0 then
                text ""

              else
                text num
            ]
        ]


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )
