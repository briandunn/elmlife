import Html exposing (Html, table, tr, td, text)
import Time exposing (Time, millisecond, every)
import Grid exposing (..)


type Msg = Tick Time

subscriptions model = every (250 * millisecond) Tick

init = (blinker, Cmd.none)

view model =
  table [] (List.map (\row ->
    tr [] (List.map (\cell ->
      td [] [text (toString cell)]) row
    )
  ) (rows model))

nextCell alive liveNeighborCount = (alive && liveNeighborCount == 2) || (liveNeighborCount == 3)

next grid = {grid | cells = List.map (\(cell, neighbors) -> nextCell cell (List.length (List.filter identity neighbors)) ) (neighbors grid)}

update msg model =
  (case msg of
    Tick time -> next model, Cmd.none)

main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
