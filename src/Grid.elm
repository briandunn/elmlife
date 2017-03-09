module Grid exposing (Grid, rows, neighbors, at, Address, empty, update, fill, height, cellCount, square, new, resize, width, map)

import Array exposing (Array, toList, fromList, get)
import List exposing (indexedMap, filterMap, concatMap, filter, length, foldl)


type alias Grid a =
    { width : Int, cells : Array a }


type alias Address =
    { row : Int, col : Int }


map : (a -> a) -> Grid a -> Grid a
map f grid =
    { grid | cells = Array.map f grid.cells }


width : Grid a -> Int
width grid =
    grid.width


cellCount : Grid a -> Int
cellCount grid =
    Array.length grid.cells


square : List a -> Grid a
square cells =
    Grid (round ((toFloat (length cells)) ^ 0.5)) <| fromList cells


new : Int -> a -> Grid a
new i value =
    Grid i (Array.repeat (i ^ 2) value)


applyOffset : Int -> Address -> Address
applyOffset i a =
    Address (a.row + i) (a.col + i)


resize : Int -> a -> Grid a -> Grid a
resize width default grid =
    let
        offset =
            ((width - grid.width) // 2) |> applyOffset

        addresses =
            grid.cells
                |> toList
                |> indexedMap (\i cell -> ( cell, offset <| toAddress i grid ))
                |> filter
                    (\( cell, address ) ->
                        (List.all (\c -> c >= 0 && c < width) [ address.row, address.col ]) && (cell /= default)
                    )
    in
        foldl (\( value, address ) grid -> set address value grid) (new width default) addresses


rows : Grid a -> List (List a)
rows grid =
    toList <|
        Array.initialize (height grid)
            (\i ->
                toList <| Array.slice (i * grid.width) ((i * grid.width) + grid.width) grid.cells
            )


neighbors : Grid a -> List ( a, List a )
neighbors grid =
    indexedMap (\i cell -> ( cell, (cellNeighbors i grid) )) <| toList grid.cells


at : Address -> Grid a -> Maybe a
at address grid =
    if address.col > ((width grid) - 1) || address.col < 0 then
        Nothing
    else if address.row > ((height grid) - 1) || address.row < 0 then
        Nothing
    else
        get ((grid.width * address.row) + address.col) grid.cells


height : Grid a -> Int
height grid =
    if grid.width > 0 then
        (Array.length grid.cells) // grid.width
    else
        0


cellNeighbors i grid =
    filterMap (\address -> (at address grid)) (neighborAddresses i grid)


toAddress : Int -> Grid a -> Address
toAddress i grid =
    if grid.width > 0 then
        Address (i // grid.width) (i % grid.width)
    else
        Address 0 0


fromAddress : Address -> Grid a -> Int
fromAddress address grid =
    address.row * grid.width + address.col


neighborAddresses : Int -> Grid a -> List Address
neighborAddresses i grid =
    let
        cell =
            toAddress i grid

        rows =
            [ cell.row - 1, cell.row, cell.row + 1 ]

        cols =
            [ cell.col - 1, cell.col, cell.col + 1 ]
    in
        filter (\address -> not <| address == cell) <| concatMap (\row -> (List.map (\col -> (Address row col)) cols)) rows


empty : Grid a
empty =
    Grid 0 <| fromList []


fill : a -> Grid a -> Grid a
fill a grid =
    Grid grid.width (Array.repeat (Array.length grid.cells) a)


set : Address -> a -> Grid a -> Grid a
set address value grid =
    { grid | cells = Array.set (fromAddress address grid) value grid.cells }


update : Int -> (a -> a) -> Grid a -> Grid a
update i f grid =
    case Array.get i grid.cells of
        Just a ->
            { grid | cells = Array.set i (f a) grid.cells }

        Nothing ->
            grid
