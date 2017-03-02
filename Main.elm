import Html exposing (Html, table, tr, td, text, aside, button, main_)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import Time exposing (Time, millisecond, every)
import Grid exposing (..)
import Random


type Msg = Tick Time | RandomCells (List Bool) | TogglePlay | Randomize

subscriptions model = every (200 * millisecond) Tick

randomize = Random.generate RandomCells (Random.list (30 ^ 2) Random.bool)

init = ({play = True, grid = (Grid 0 [])}, randomize)

view model =
  main_ [] [
    aside [] [
        button [onClick TogglePlay] [text "|>"],
        button [onClick Randomize] [text "rand"]
    ],
    table [] (List.map (\row ->
      tr [] (List.map (\cell ->
        td [classList [("live", cell)]] []) row
      )
    ) (rows model.grid))
  ]

nextCell alive liveNeighborCount = (alive && liveNeighborCount == 2) || (liveNeighborCount == 3)

next grid = {grid | cells = List.map (\(cell, neighbors) -> nextCell cell (List.length (List.filter identity neighbors)) ) (neighbors grid)}

update msg model =
    case msg of
        Tick time -> (if model.play then {model | grid = (next model.grid)} else model, Cmd.none)
        RandomCells cells -> ({model | grid = (Grid 30 cells)}, Cmd.none)
        TogglePlay -> ({model | play = not model.play}, Cmd.none)
        Randomize -> (model, randomize)

main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
