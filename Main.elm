module Main exposing (main)

import Html exposing (Html, table, tr, td, text, aside, button, main_, input, label, li, ul)
import Html.Attributes as Attr exposing (classList, type_, step, value)
import Html.Events exposing (onClick, onInput, onMouseDown)
import Time exposing (Time, millisecond, every)
import Grid exposing (..)
import Random


type alias Model =
    { play : Bool, grid : Grid Bool, speed : Int, frames : List (Grid Bool) }


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
    every
        (model.speed
            |> (*) 250
            |> abs
            |> (-) 1001
            |> toFloat
            |> (*) millisecond
        )
        Tick


randomize grid =
    Random.generate RandomCells (Random.list (Grid.cellCount grid) Random.bool)


init : ( Model, Cmd Msg )
init =
    let
        grid =
            Grid.new 30 False
    in
        ( { play = True, grid = grid, speed = 1, frames = [] }, (randomize grid) )


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
                            , Attr.min "-4"
                            , Attr.max "4"
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


tick : Model -> Model
tick model =
    if model.play then
        if model.speed > 0 then
            let
                next =
                    nextGrid model.grid
            in
                if next /= model.grid then
                    { model | grid = next, frames = model.grid :: model.frames }
                else
                    { model | play = False }
        else
            { model
                | grid = Maybe.withDefault model.grid (List.head model.frames)
                , frames = Maybe.withDefault [] (List.tail model.frames)
            }
    else
        model


update msg model =
    let
        stringToInt =
            updateFromIntString model
    in
        case msg of
            Tick time ->
                ( tick model, Cmd.none )

            RandomCells cells ->
                ( { model | grid = Grid.square cells }, Cmd.none )

            TogglePlay ->
                ( { model | play = not model.play }, Cmd.none )

            Randomize ->
                ( model, (randomize model.grid) )

            CellClick i ->
                ( { model | grid = (Grid.update i not model.grid), play = False }, Cmd.none )

            Clear ->
                ( { model | grid = Grid.fill False model.grid, frames = [], play = False }, Cmd.none )

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
