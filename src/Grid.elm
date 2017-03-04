module Grid exposing (Grid, rows, neighbors, at, Address, empty, update, fill, height, cellCount, square, new)

import List exposing (take, drop, indexedMap, head, filterMap, map, concatMap, filter, length, repeat)


type alias Grid a =
    { width : Int, cells : List a }


type alias Address =
    { row : Int, col : Int }


cellCount : Grid a -> Int
cellCount grid =
    length grid.cells


square : List a -> Grid a
square cells =
    Grid (round ((toFloat (length cells)) ^ 0.5)) cells


new : Int -> a -> Grid a
new i value =
    Grid i (repeat (i ^ 2) value)


rows : Grid a -> List (List a)
rows grid =
    groupsOf grid.width grid.cells


groupsOf size list =
    let
        rest =
            drop size list
    in
        (take size list)
            :: (if rest == [] then
                    []
                else
                    (groupsOf size rest)
               )


neighbors : Grid a -> List ( a, List a )
neighbors grid =
    indexedMap (\i cell -> ( cell, (cellNeighbors i grid) )) grid.cells


at : Address -> Grid a -> Maybe a
at address grid =
    if address.col > (grid.width - 1) || address.col < 0 then
        Nothing
    else if address.row > ((height grid) - 1) || address.row < 0 then
        Nothing
    else
        head <| drop ((grid.width * address.row) + address.col) grid.cells


height : Grid a -> Int
height grid =
    if grid.width > 0 then
        (length grid.cells) // grid.width
    else
        0


cellNeighbors i grid =
    filterMap (\address -> (at address grid)) (neighborAddresses i grid)


toAddress i grid =
    if grid.width > 0 then
        Address (i // grid.width) (i % grid.width)
    else
        Address 0 0


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
        filter (\address -> not <| address == cell) <| concatMap (\row -> (map (\col -> (Address row col)) cols)) rows


empty : Grid a
empty =
    Grid 0 []


fill : a -> Grid a -> Grid a
fill a grid =
    Grid grid.width (repeat (length grid.cells) a)


update : Int -> (a -> a) -> Grid a -> Grid a
update i f grid =
    let
        rest =
            (drop i grid.cells)
    in
        { grid | cells = (take i grid.cells) ++ (map f (filterMap head [ rest ])) ++ (drop 1 rest) }
