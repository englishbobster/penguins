module Helpers
    exposing
        ( AxialCoord
        , convertFromEvenQToAxial
        , axialCoordsToPixel
        , pixelToAxialCoords
        , calculateRoute
        , nearestNeighbours
        )

import Hexagon exposing (PixelCoord)


type alias AxialCoord =
    ( Int, Int )


type alias EvenQ =
    ( Int, Int )


convertFromEvenQToAxial : EvenQ -> AxialCoord
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


nearestNeighbours : AxialCoord -> List AxialCoord
nearestNeighbours ( centerx, centery ) =
    let
        directionList =
            [ ( 1, -1 ), ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( 0, -1 ) ]
    in
        List.map (\( x, y ) -> ( (centerx + x), (centery + y) )) directionList


calculateDistanceBetweenCenters : AxialCoord -> AxialCoord -> Int
calculateDistanceBetweenCenters ( startx, starty ) ( finishx, finishy ) =
    let
        startz =
            0 - startx - starty

        finishz =
            0 - finishx - finishy

        dx =
            abs (startx - finishx)

        dy =
            abs (starty - finishy)

        dz =
            abs (startz - finishz)
    in
        Maybe.withDefault 0 (List.maximum [ dx, dy, dz ])


calculateRoute : AxialCoord -> AxialCoord -> List AxialCoord
calculateRoute start finish =
    let
        distance =
            calculateDistanceBetweenCenters start finish
    in
        start :: interpolateRoute start finish distance distance []


interpolateRoute : AxialCoord -> AxialCoord -> Int -> Int -> List AxialCoord -> List AxialCoord
interpolateRoute start finish distance step currentRoute =
    if step == 0 then
        currentRoute
    else
        let
            ( startx, starty ) =
                start

            ( finishx, finishy ) =
                finish

            lerp : Float -> Float -> Float
            lerp a b =
                a + (b - a) * (1 / (toFloat distance)) * step

            next =
                ( (lerp (toFloat startx) (toFloat finishx))
                , (lerp (toFloat starty) (toFloat finishy))
                )
                    |> roundAxialHex
        in
            interpolateRoute start finish distance (step - 1) (next :: currentRoute)


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
