module Hexagon
    exposing
        ( HexModel
        , Coord
        , Msg(..)
        , updateHex
        , hexagonFace
        )

import String exposing (join)
import Svg exposing (Svg, svg, text', text, polygon)
import Svg.Attributes exposing (style, points, x, y, fontSize, dy)
import Svg.Events exposing (onMouseOver)


type alias Coord =
    ( Float, Float )


type alias HexModel =
    { border : String
    , size : Int
    , colour : String
    , value : Int
    , center : Coord
    , shrinkFactor : Int
    }


type Msg
    = Something Coord


updateHex : Msg -> HexModel -> HexModel
updateHex msg model =
    case msg of
        Something pos ->
            model


hexagonFace : HexModel -> Svg Msg
hexagonFace model =
    svg []
        [ polygon
            [ points (hexagonPoints model.center (model.size - model.shrinkFactor))
            , hexColour model.colour model.border
            , onMouseOver (Something model.center)
            ]
            []
        , hexagonValue model.center model.value
        ]


hexagonValue : Coord -> Int -> Svg msg
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
