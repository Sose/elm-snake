module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias Coordinate =
    ( Int, Int )


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Snake =
    { elems : List Coordinate
    , direction : Direction
    }


type alias Model =
    { apple : Coordinate
    , snake : Snake
    }


type Msg
    = ButtonClick


cellDimensions =
    { width = "20px", height = "20px" }


boardDimensions : Coordinate
boardDimensions =
    ( 30, 20 )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { apple = ( 15, 15 )
      , snake =
            { direction = Left
            , elems = [ ( 10, 10 ), ( 11, 10 ), ( 12, 10 ), ( 13, 10 ), ( 14, 10 ) ]
            }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ButtonClick ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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
