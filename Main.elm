import Html exposing (Html, table, tr, td, text)
import Html.Attributes exposing (classList)
import Time exposing (Time, millisecond, every)
import Grid exposing (..)
import Random


type Msg = Tick Time | RandomCells (List Bool)

subscriptions model = every (200 * millisecond) Tick

init = ((Grid 0 []),(Random.generate RandomCells (Random.list (30 ^ 2) Random.bool)))

view model =
  table [] (List.map (\row ->
    tr [] (List.map (\cell ->
      td [classList [("live", cell)]] []) row
    )
  ) (rows model))

nextCell alive liveNeighborCount = (alive && liveNeighborCount == 2) || (liveNeighborCount == 3)

next grid = {grid | cells = List.map (\(cell, neighbors) -> nextCell cell (List.length (List.filter identity neighbors)) ) (neighbors grid)}

update msg model =
    case msg of
        Tick time -> (next model, Cmd.none)
        RandomCells cells -> ((Grid 30 cells), Cmd.none)

main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
