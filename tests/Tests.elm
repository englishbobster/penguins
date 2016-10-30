module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Hexagon
import Penguins
import Helpers


all : Test
all =
    describe "A Test Suite"
        [ test "Should produce a hexagon map with height 10 and width 10" <|
            \() ->
                Expect.equal (Penguins.generateMapKeys 10 10 |> List.length)
                    100
        , test "first key in generated keys should be (0,0)" <|
            \() ->
                Expect.equal (List.take 1 (Penguins.generateMapKeys 6 6))
                    [ ( 0, 0 ) ]
        , test "13th key should be (2, -1)" <|
            \() ->
                Expect.equal (List.drop 12 (Penguins.generateMapKeys 6 6) |> List.take 1)
                    [ ( 2, -1 ) ]
        , test "last key in generated keys should be (5,3)" <|
            \() ->
                Expect.equal (List.take 1 (Penguins.generateMapKeys 6 6 |> List.reverse))
                    [ ( 5, 3 ) ]
        , test "calculated route should have a length of 6 including start and finish" <|
            \() ->
                Expect.equal (Helpers.calculateRoute ( 3, 3 ) ( 3, 8 ) |> List.length)
                    6
        , test "interpolated route gives correct route" <|
            \() ->
                Expect.equal (Helpers.calculateRoute ( 3, 3 ) ( 3, 8 ))
                    [ ( 3, 3 ), ( 3, 4 ), ( 3, 5 ), ( 3, 6 ), ( 3, 7 ), ( 3, 8 ) ]
        , test "interpolated route gives another correct route" <|
            \() ->
                Expect.equal (Helpers.calculateRoute ( 4, 1 ) ( 8, 1 ))
                    [ ( 4, 1 ), ( 5, 1 ), ( 6, 1 ), ( 7, 1 ), ( 8, 1 ) ]
        ]
