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
                Expect.equal (Penguins.generateMapKeys 10 10 |> List.length) 100
        , test "first key in generated keys should be (0,0)" <|
            \() ->
                Expect.equal (List.take 1 (Penguins.generateMapKeys 6 6)) [ ( 0, 0 ) ]
        , test "13th key should be (2, -1)" <|
            \() ->
                Expect.equal (List.drop 12 (Penguins.generateMapKeys 6 6) |> List.take 1) [ ( 2, -1 ) ]
        , test "last key in generated keys should be (5,3)" <|
            \() ->
                Expect.equal (List.take 1 (Penguins.generateMapKeys 6 6 |> List.reverse)) [ ( 5, 3 ) ]
        , test "should calculate the distance between (3, 3) and (3, 8)" <|
            \() ->
                Expect.equal (Helpers.calculateDistanceBetweenCenters ( 3, 3 ) ( 3, 8 )) 5
        ]
