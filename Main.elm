module Main exposing (main)

import Html exposing (Html, table, tr, td, text, aside, button, main_, input, label, li, ul)
import Html.Attributes exposing (classList, type_)
import Html.Events exposing (onClick, onInput)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    every (toFloat ((101 - model.speed) * 10) * millisecond) Tick


randomize =
    Random.generate RandomCells (Random.list (30 ^ 2) Random.bool)


init : ( Model, Cmd Msg )
init =
    ( { play = True, grid = (Grid 0 []), speed = 50 }, randomize )


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
                    [ label []
                        [ input [ type_ "range", onInput Speed ] [], text "speed" ]
                    ]
                ]
            ]
        , table []
            (List.map
                (\row ->
                    tr []
                        (List.map
                            (\cell ->
                                td [ classList [ ( "live", cell ) ] ] []
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
