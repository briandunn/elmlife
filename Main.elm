module Main exposing (main)

import Html exposing (Html, table, tr, td, text, aside, button, main_, input, label, li, ul)
import Html.Attributes as Attr exposing (classList, type_, step, value)
import Html.Events exposing (onClick, onInput, onMouseDown)
import Time exposing (Time, millisecond, every)
import Grid exposing (..)
import Random


type alias Model =
    { play : Bool, grid : Grid Bool, speed : Int }


type Msg
    = Tick Time
    | RandomCells (List Bool)
    | TogglePlay
    | Randomize
    | Speed String
    | Size String
    | CellClick Int
    | Clear


subscriptions : Model -> Sub Msg
subscriptions model =
    every ((toFloat (1001 - model.speed)) * millisecond) Tick


randomize grid =
    Random.generate RandomCells (Random.list (Grid.cellCount grid) Random.bool)


init : ( Model, Cmd Msg )
init =
    let
        grid =
            Grid.new 30 False
    in
        ( { play = True, grid = grid, speed = 500 }, (randomize grid) )


view model =
    main_ []
        [ aside []
            [ ul []
                [ li []
                    [ button [ onClick TogglePlay ]
                        [ text <|
                            if model.play then
                                "pause"
                            else
                                "play"
                        ]
                    ]
                , li []
                    [ button [ onClick Randomize ]
                        [ text "rand" ]
                    ]
                , li []
                    [ button [ onClick Clear ]
                        [ text "clear" ]
                    ]
                , li []
                    [ label []
                        [ input
                            [ type_ "range"
                            , onInput Speed
                            , Attr.min "0"
                            , Attr.max "1000"
                            , step "100"
                            , value (toString model.speed)
                            ]
                            []
                        , text "speed"
                        ]
                    ]
                , li []
                    [ label []
                        [ input
                            [ type_ "range"
                            , onInput Size
                            , Attr.min "3"
                            , Attr.max "75"
                            , value (toString (Grid.width model.grid))
                            ]
                            []
                        , text "size"
                        ]
                    ]
                ]
            ]
        , table []
            (List.indexedMap
                (\y row ->
                    tr []
                        (List.indexedMap
                            (\x cell ->
                                td
                                    [ classList [ ( "live", cell ) ]
                                    , onMouseDown (CellClick (y * model.grid.width + x))
                                    ]
                                    []
                            )
                            row
                        )
                )
                (rows model.grid)
            )
        ]


nextCell : Bool -> Int -> Bool
nextCell alive liveNeighborCount =
    (alive && liveNeighborCount == 2) || (liveNeighborCount == 3)


nextGrid : Grid Bool -> Grid Bool
nextGrid grid =
    Grid.square <| List.map (\( cell, neighbors ) -> nextCell cell (List.length (List.filter identity neighbors))) (neighbors grid)


update msg model =
    let
        stringToInt =
            updateFromIntString model
    in
        case msg of
            Tick time ->
                ( if model.play then
                    { model | grid = (nextGrid model.grid) }
                  else
                    model
                , Cmd.none
                )

            RandomCells cells ->
                ( { model | grid = Grid.square cells }, Cmd.none )

            TogglePlay ->
                ( { model | play = not model.play }, Cmd.none )

            Randomize ->
                ( model, (randomize model.grid) )

            CellClick i ->
                ( { model | grid = (Grid.update i not model.grid), play = False }, Cmd.none )

            Clear ->
                ( { model | grid = Grid.fill False model.grid }, Cmd.none )

            Speed value ->
                stringToInt value (\int -> { model | speed = int })

            Size value ->
                stringToInt value (\int -> { model | grid = Grid.resize int False model.grid })


updateFromIntString : Model -> String -> (Int -> Model) -> ( Model, Cmd Msg )
updateFromIntString model value f =
    case String.toInt value of
        Ok int ->
            ( (f int), Cmd.none )

        Err _ ->
            ( model, Cmd.none )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
