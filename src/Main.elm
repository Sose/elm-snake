module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Time


type alias Coordinate =
    ( Int, Int )


type Direction
    = Up
    | Down
    | Left
    | Right
    | Other


type alias Snake =
    { elems : List Coordinate
    , direction : Direction
    }


type alias Model =
    { apple : Coordinate
    , snake : Snake
    , timeSinceUpdate : Float
    }


type Msg
    = Tick Float
    | KeyDown Direction


addCoord : Coordinate -> Coordinate -> Coordinate
addCoord ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


dirToDeltas : Direction -> Coordinate
dirToDeltas d =
    case d of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )

        _ ->
            ( 0, 0 )


updateInterval : Float
updateInterval =
    700


cellDimensions =
    { width = "20px", height = "20px" }


boardDimensions : Coordinate
boardDimensions =
    ( 30, 20 )


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection str =
    case str of
        "ArrowLeft" ->
            KeyDown Left

        "ArrowRight" ->
            KeyDown Right

        "ArrowUp" ->
            KeyDown Up

        "ArrowDown" ->
            KeyDown Down

        _ ->
            KeyDown Other


advanceSnake : Snake -> Snake
advanceSnake s =
    let
        oldHead =
            case List.head s.elems of
                Just x ->
                    x

                Nothing ->
                    ( 0, 0 )

        newHead =
            addCoord oldHead (dirToDeltas s.direction)

        len =
            List.length s.elems

        newElems =
            newHead :: List.take (len - 1) s.elems
    in
    { s | elems = newElems }


tick : Model -> Float -> Model
tick model t =
    if model.timeSinceUpdate + t >= updateInterval then
        { model | timeSinceUpdate = 0, snake = advanceSnake model.snake }

    else
        { model | timeSinceUpdate = model.timeSinceUpdate + t }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { apple = ( 15, 15 )
      , snake =
            { direction = Right
            , elems = [ ( 14, 10 ), ( 13, 10 ), ( 12, 10 ), ( 11, 10 ), ( 10, 10 ) ]
            }
      , timeSinceUpdate = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( tick model t, Cmd.none )

        KeyDown dir ->
            let
                allowedDir : Direction -> Direction -> Bool
                allowedDir snakeDir d =
                    case ( snakeDir, d ) of
                        ( Left, Right ) ->
                            False

                        ( Right, Left ) ->
                            False

                        ( Up, Down ) ->
                            False

                        ( Down, Up ) ->
                            False

                        _ ->
                            True

                updatedSnake snake =
                    if allowedDir snake.direction dir then
                        { snake | direction = dir }

                    else
                        snake
            in
            ( { model | snake = updatedSnake model.snake }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyDown keyDecoder
        ]


view : Model -> Html Msg
view model =
    let
        colorForCoordinate : Coordinate -> String
        colorForCoordinate coord =
            if coord == model.apple then
                "red"

            else if List.member coord model.snake.elems then
                "blue"

            else
                "white"

        viewRow y =
            div [ style "display" "table-row" ]
                (List.map
                    (\x ->
                        div
                            [ style "display" "table-cell"
                            , style "height" cellDimensions.height
                            , style "width" cellDimensions.width
                            , style "background-color" (colorForCoordinate ( x, y ))
                            ]
                            []
                    )
                    (List.range 0 (Tuple.first boardDimensions))
                )

        viewBoard =
            div [ style "display" "table" ]
                (List.map
                    (\y -> viewRow y)
                    (List.range 0 (Tuple.second boardDimensions))
                )
    in
    div
        [ style "background-color" "black"
        , style "height" "100vh"
        , style "padding-left" "20px"
        , style "padding-top" "20px"
        ]
        [ viewBoard ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
