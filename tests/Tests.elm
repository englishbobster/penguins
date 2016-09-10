module Tests exposing (..)

import Test exposing (..)
import Expect
import String

import Hexagon

all : Test
all =
    describe "A Test Suite"
        [ test "Should produde a string of points for a hexagon" <|
            \() ->
                Expect.equal ( Hexagon.hexagonPoints (50, 50) 50 ) "75,93,25,93,0,50,25,7,75,7,100,50"
        ]
