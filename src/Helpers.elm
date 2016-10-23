module Helpers
    exposing
        ( AxialCoord
        , convertFromEvenQToAxial
        , axialCoordsToPixel
        , pixelToAxialCoords
        )

import Hexagon exposing (PixelCoord)


type alias AxialCoord =
    ( Int, Int )


convertFromEvenQToAxial : ( Int, Int ) -> AxialCoord
convertFromEvenQToAxial ( col, row ) =
    let
        x =
            col

        z =
            row - (col - (col % 2)) // 2

        y =
            0 - x - z
    in
        ( x, z )


axialCoordsToPixel : Int -> AxialCoord -> PixelCoord
axialCoordsToPixel size ( q, r ) =
    let
        offset =
            (toFloat (size * 2))

        x =
            (toFloat size) * (3 / 2) * (toFloat q)

        y =
            (toFloat size) * (sqrt 3) * ((toFloat r) + ((toFloat q) / 2))
    in
        ( offset + x, offset + y )


pixelToAxialCoords : Int -> PixelCoord -> AxialCoord
pixelToAxialCoords size ( x, y ) =
    let
        offset =
            (toFloat (size * 2))

        ox =
            x - offset

        oy =
            y - offset

        q =
            (ox * 2 / 3) / (toFloat size)

        r =
            ((-ox / 3) + (((sqrt 3) / 3) * oy)) / (toFloat size)
    in
        ( q, r ) |> roundAxialHex


roundAxialHex : PixelCoord -> AxialCoord
roundAxialHex ( x, y ) =
    let
        z =
            0 - x - y

        rx =
            round x

        ry =
            round y

        rz =
            round (0 - x - y)

        xdiff =
            abs ((toFloat rx) - x)

        ydiff =
            abs ((toFloat ry) - y)

        zdiff =
            abs ((toFloat rz) - z)
    in
        if xdiff > ydiff && xdiff > zdiff then
            ( (0 - ry - rz), ry )
        else if ydiff > zdiff then
            ( rx, (0 - rx - rz) )
        else
            ( rx, ry )
