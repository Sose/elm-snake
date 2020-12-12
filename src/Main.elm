module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random
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
    | NewApple Coordinate


eqCoord : Coordinate -> Coordinate -> Bool
eqCoord ( x1, y1 ) ( x2, y2 ) =
    x1 == x2 && y1 == y2


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


type alias Config =
    { updateInterval : Float
    , cellDimensions : Coordinate
    , boardDimensions : Coordinate
    }


config : Config
config =
    { updateInterval = 500
    , cellDimensions = ( 20, 20 )
    , boardDimensions = ( 30, 20 )
    }


randomPosition : Coordinate -> Coordinate
randomPosition dimensions =
    ( 5, 5 )


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


randomCoordinate : Coordinate -> Random.Generator Coordinate
randomCoordinate ( maxX, maxY ) =
    Random.map2
        (\x y -> ( x, y ))
        (Random.int 0 maxX)
        (Random.int 0 maxY)


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


advanceSnake : Model -> ( Model, Cmd Msg )
advanceSnake model =
    let
        s =
            model.snake

        oldHead =
            case List.head s.elems of
                Just x ->
                    x

                Nothing ->
                    ( 0, 0 )

        newHead =
            addCoord oldHead (dirToDeltas s.direction)

        hitApple =
            eqCoord model.apple newHead

        len =
            if hitApple then
                List.length s.elems

            else
                List.length s.elems - 1

        newElems =
            newHead :: List.take len s.elems

        updatedSnake snake =
            { snake | elems = newElems }

        updatedApple =
            if hitApple then
                randomPosition config.boardDimensions

            else
                model.apple

        cmd =
            if hitApple then
                let
                    maxX =
                        Tuple.first config.boardDimensions

                    maxY =
                        Tuple.second config.boardDimensions
                in
                Random.generate NewApple (randomCoordinate ( maxX, maxY ))

            else
                Cmd.none
    in
    ( { model | timeSinceUpdate = 0, snake = updatedSnake s, apple = updatedApple }, cmd )


tick : Model -> Float -> ( Model, Cmd Msg )
tick model t =
    if model.timeSinceUpdate + t >= config.updateInterval then
        advanceSnake model

    else
        ( { model | timeSinceUpdate = model.timeSinceUpdate + t }, Cmd.none )


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
            tick model t

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

        NewApple coords ->
            ( { model | apple = coords }, Cmd.none )


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

        width ( x, _ ) =
            String.fromInt x ++ "px"

        height ( _, y ) =
            String.fromInt y ++ "px"

        viewRow y =
            div [ style "display" "table-row" ]
                (List.map
                    (\x ->
                        div
                            [ style "display" "table-cell"
                            , style "height" (height config.cellDimensions)
                            , style "width" (width config.cellDimensions)
                            , style "background-color" (colorForCoordinate ( x, y ))
                            ]
                            []
                    )
                    (List.range 0 (Tuple.first config.boardDimensions))
                )

        viewBoard =
            div [ style "display" "table" ]
                (List.map
                    (\y -> viewRow y)
                    (List.range 0 (Tuple.second config.boardDimensions))
                )
    in
    div
        [ style "background-color" "black"
        , style "height" "80vh"
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
