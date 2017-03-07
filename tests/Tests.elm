module Tests exposing (..)

import Test exposing (..)
import Expect
import Grid exposing (..)


all : Test
all =
    describe "Grid"
        [ test "breaks into rows" <|
            \() ->
                Expect.equal (rows (Grid 3 (List.repeat 6 1))) [ [ 1, 1, 1 ], [ 1, 1, 1 ] ]
        , test "looks up by row and col" <|
            \() ->
                Expect.equal
                    (at (Address -1 -1)
                        (Grid 2
                            [ 0
                            , 1
                            , 2
                            , 3
                            ]
                        )
                    )
                    Nothing

        , test "resizes up" <|
            \() ->
                Expect.equal
                    (resize 4 0
                        (Grid 2
                            [ 0, 1
                            , 2, 3
                            ]
                        )
                    )
                    (Grid 4 [ 0, 0, 0, 0
                            , 0, 0, 1, 0
                            , 0, 2, 3, 0
                            , 0, 0, 0, 0
                            ]
                    )

        , test "resizes down" <|
            \() ->
                Expect.equal
                    (resize 2 0
                        (Grid 4 [ 0, 0, 0, 0
                                , 0, 0, 1, 0
                                , 0, 2, 3, 0
                                , 0, 0, 0, 0
                                ]
                        )
                    )
                    (Grid 2
                        [ 0, 1
                        , 2, 3
                        ]
                    )
        , test "finds neighbors" <|
            \() ->
                Expect.equal
                    (neighbors
                        (Grid 3
                            [ 0
                            , 1
                            , 2
                            , 3
                            , 4
                            , 5
                            , 6
                            , 7
                            , 8
                            ]
                        )
                    )
                    [ ( 0, [ 1, 3, 4 ] )
                    , ( 1, [ 0, 2, 3, 4, 5 ] )
                    , ( 2, [ 1, 4, 5 ] )
                    , ( 3, [ 0, 1, 4, 6, 7 ] )
                    , ( 4, [ 0, 1, 2, 3, 5, 6, 7, 8 ] )
                    , ( 5, [ 1, 2, 4, 7, 8 ] )
                    , ( 6, [ 3, 4, 7 ] )
                    , ( 7, [ 3, 4, 5, 6, 8 ] )
                    , ( 8, [ 4, 5, 7 ] )
                    ]
        ]
