module Hexagon exposing (hexagonFace)

import String exposing (join)
import Svg exposing (Svg, svg, text', text, polygon)
import Svg.Attributes exposing (style, points, x, y, fontSize, dy)


type alias Coord =
    ( Float, Float )


hexagonFace : Coord -> Int -> String -> String -> Svg msg
hexagonFace center size colour label =
    svg []
        [ polygon [ points (hexagonPoints center size), hexColour colour ] []
        , hexagonLabel center label
        ]


hexagonLabel : Coord -> String -> Svg msg
hexagonLabel center label =
    let
        ( xcenter, ycenter ) =
            center

        attributeList =
            [ x (toString xcenter)
            , y (toString ycenter)
            , fontSize "48px"
            , dy "+15px"
            , style
                ("text-anchor:middle;"
                    ++ "font-family:Arial;"
                    ++ "stroke:black;"
                    ++ "stroke-width:2;"
                    ++ "fill:white"
                )
            ]
    in
        text' attributeList [ text label ]


hexColour : String -> Svg.Attribute msg
hexColour colour =
    style
        ("fill:"
            ++ colour
            ++ ";stroke:black;"
            ++ "stroke-width:6"
        )


hexagonPoints : Coord -> Int -> String
hexagonPoints center size =
    [0..5]
        |> List.map (hexagonCorner center size)
        |> List.map (\( x, y ) -> toString (round x) ++ "," ++ toString (round y))
        |> join ","


hexagonCorner : Coord -> Int -> Int -> Coord
hexagonCorner center size cornerIndex =
    let
        angleRad =
            Basics.degrees (60 * (toFloat cornerIndex))

        ( x, y ) =
            center
    in
        ( x + (toFloat size) * (cos angleRad), y + (toFloat size) * (sin angleRad) )
