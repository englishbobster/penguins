module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Hexagon
import Penguins


all : Test
all =
    describe "A Test Suite"
        [ test "Should produde a string of points for a hexagon" <|
            \() ->
                Expect.equal (Hexagon.hexagonPoints ( 50, 50 ) 50) "75,93,25,93,0,50,25,7,75,7,100,50"
        , test "Should produce a hexagon map with height 10 and width 10" <|
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
        ]
