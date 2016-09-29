module Hexagon exposing (..)

import String exposing (join)
import Html exposing (Html, div)
import Svg exposing (Svg, polygon)
import Svg.Attributes exposing (points)
import Html.Attributes exposing (class, rel, href)


type alias Coord =
    ( Float, Float )


drawHexagon : Coord -> Float -> String -> String -> Html msg
drawHexagon center size frontColour backColour =
    div []
        [ css "../flip.css"
        , hexagonFlipper center size frontColour backColour
        ]


css : String -> Html msg
css path =
    Html.node "link" [ rel "stylesheet", href path ] []


hexagonFlipper : Coord -> Float -> String -> String -> Html msg
hexagonFlipper center size frontColour backColour =
    div [ Html.Attributes.class "flip-container" ]
        [ div [ Html.Attributes.class "flipper" ]
            [ div [ Html.Attributes.class "front" ]
                [ hexagonFace center size frontColour ]
            , div [ Html.Attributes.class "back" ]
                [ hexagonFace center size backColour ]
            ]
        ]


hexagonFace : Coord -> Float -> String -> Svg msg
hexagonFace center size colour =
    Svg.svg [] [ polygon [ points (hexagonPoints center size), hexColour colour ] [] ]


hexColour : String -> Svg.Attribute msg
hexColour colour =
    Svg.Attributes.style ("fill:" ++ colour ++ ";stroke:black;stroke-width:6")


hexagonPoints : Coord -> Float -> String
hexagonPoints center size =
    [0..5]
        |> List.map (hexCorner center size)
        |> List.map (\( x, y ) -> toString (round x) ++ "," ++ toString (round y))
        |> join ","


hexCorner : Coord -> Float -> Int -> Coord
hexCorner center size cornerIndex =
    let
        angleRad =
            Basics.degrees (60 * (Basics.toFloat cornerIndex) + 60)
    in
        ( Basics.fst center + size * (cos angleRad), Basics.snd center + size * (sin angleRad) )
