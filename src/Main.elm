module Main exposing (..)

import Hexagon exposing (drawHexagon)
import Hexagons.Map exposing (Map)


type alias Model =
    { map : Map }


main =
    drawHexagon (60,60) 50 "cyan" "yellow"
