module Hexagon exposing (..)

import String exposing (..)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Coord =
    (Float, Float)


view : Html Cmd
view =
    div [] [ hexagon (60, 60) 50 "blue" ]


hexColour: String -> Svg.Attribute msg
hexColour colour =
    Svg.Attributes.style ( "fill:" ++ colour ++ ";stroke:black;stroke-width:3" )


hexagon : Coord -> Float -> String -> Svg Cmd
hexagon center size colour =
    Svg.svg [] [ polygon [points ( hexagonPoints center size ), hexColour colour] [] ]


hexCorner : Coord -> Float -> Int -> Coord
hexCorner center size cornerIndex =
    let
        angleRad = Basics.degrees (60 * (Basics.toFloat cornerIndex) + 60)
    in
        (fst center + size * (cos angleRad), snd center + size *(sin angleRad))


hexagonPoints : Coord -> Float -> String
hexagonPoints center size =
            [0,1,2,3,4,5]
            |> List.map (hexCorner center size)
            |> List.map (\(x,y) -> toString (round x) ++ "," ++ toString (round y))
            |> join ","


main : Html Cmd
main =
    view
