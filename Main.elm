module Main exposing (main)

import Html exposing (Html, table, tr, td, text, aside, button, main_, input, label, li, ul)
import Html.Attributes exposing (classList, type_)
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
    | CellClick Int
    | Clear


subscriptions : Model -> Sub Msg
subscriptions model =
    every (toFloat ((101 - model.speed) * 10) * millisecond) Tick


randomize =
    Random.generate RandomCells (Random.list (30 ^ 2) Random.bool)


init : ( Model, Cmd Msg )
init =
    ( { play = True, grid = (Grid.empty), speed = 50 }, randomize )


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
                        [ input [ type_ "range", onInput Speed ] [], text "speed" ]
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


nextCell alive liveNeighborCount =
    (alive && liveNeighborCount == 2) || (liveNeighborCount == 3)


next grid =
    { grid | cells = List.map (\( cell, neighbors ) -> nextCell cell (List.length (List.filter identity neighbors))) (neighbors grid) }


update msg model =
    case msg of
        Tick time ->
            ( if model.play then
                { model | grid = (next model.grid) }
              else
                model
            , Cmd.none
            )

        RandomCells cells ->
            ( { model | grid = (Grid 30 cells) }, Cmd.none )

        TogglePlay ->
            ( { model | play = not model.play }, Cmd.none )

        Randomize ->
            ( model, randomize )

        CellClick i ->
            ( { model | grid = (Grid.update i not model.grid) }, Cmd.none )

        Clear ->
            ( { model | grid = Grid.fill False model.grid }, Cmd.none )

        Speed value ->
            case String.toInt (value) of
                Ok int ->
                    ( { model | speed = int }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
