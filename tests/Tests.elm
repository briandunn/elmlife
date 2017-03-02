module Tests exposing (..)

import Test exposing (..)
import Expect
import Grid exposing(..)

all : Test
all =
    describe "rows"
        [ test "breaks into rows" <|
            \() ->
                Expect.equal (rows (Grid 3 (List.repeat 6 1))) [[1,1,1],[1,1,1]]
        , test "looks up by row and col" <|
            \() ->
                Expect.equal (at (Address -1 -1) (Grid 2 [0,1,
                                                        2,3])) Nothing
        , test "finds neighbors" <|
            \() ->
                Expect.equal (neighbors (Grid 3 [0,1,2,
                                                 3,4,5,
                                                 6,7,8])) [(0, [1,3,4])
                                                          ,(1, [0,2,3,4,5])
                                                          ,(2, [1,4,5])
                                                          ,(3, [0,1,4,6,7])
                                                          ,(4, [0,1,2,3,5,6,7,8])
                                                          ,(5, [1,2,4,7,8])
                                                          ,(6, [3,4,7])
                                                          ,(7, [3,4,5,6,8])
                                                          ,(8, [4,5,7])]
            ]

{-

import String
import Fuzz exposing (list, int, tuple, string)
all : Test
all =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            , test "This test should fail - you should remove it" <|
                \() ->
                    Expect.fail "Failed as expected!"
            ]
        , describe "Fuzz test examples, using randomly generated input"
            [ fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            , fuzz (list int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]
        ]
-}
