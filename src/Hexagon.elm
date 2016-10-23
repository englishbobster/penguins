module Hexagon
    exposing
        ( HexModel
        , PixelCoord
        , hexagon
        )

import String exposing (join)
import Svg exposing (Svg, svg, text', text, polygon)
import Svg.Attributes exposing (style, points, x, y, fontSize, dy)


type alias PixelCoord =
    ( Float, Float )


type alias HexModel =
    { border : String
    , size : Int
    , colour : String
    , value : Int
    , center : PixelCoord
    , occupied : Bool
    , shrinkFactor : Int
    }


hexagon : HexModel -> Svg msg
hexagon model =
    svg []
        [ polygon
            [ points (hexagonPoints model.center (model.size - model.shrinkFactor))
            , hexColour model.colour model.border
            ]
            []
        , hexagonValue model.center model.value
        ]


hexagonValue : PixelCoord -> Int -> Svg msg
hexagonValue center value =
    let
        ( xcenter, ycenter ) =
            center

        label =
            toString value

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


hexColour : String -> String -> Svg.Attribute msg
hexColour colour borderColour =
    style
        ("fill:"
            ++ colour
            ++ ";stroke:"
            ++ borderColour
            ++ ";stroke-width:6"
        )


hexagonPoints : PixelCoord -> Int -> String
hexagonPoints center size =
    [0..5]
        |> List.map (hexagonCorner center size)
        |> List.map (\( x, y ) -> toString (round x) ++ "," ++ toString (round y))
        |> join ","


hexagonCorner : PixelCoord -> Int -> Int -> PixelCoord
hexagonCorner center size cornerIndex =
    let
        angleRad =
            Basics.degrees (60 * (toFloat cornerIndex))

        ( x, y ) =
            center
    in
        ( x + (toFloat size) * (cos angleRad), y + (toFloat size) * (sin angleRad) )
