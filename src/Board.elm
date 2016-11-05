module Board
    exposing
        ( Board
        , Route
        , generateBoard
        , generateMapKeys
        , deleteRouteFromBoard
        , findRouteOnBoard
        , scoreRoute
        )

import Hexagon exposing (HexModel, emptyHexagon)
import Helpers exposing (AxialCoord, convertFromEvenQToAxial, axialCoordsToPixel)
import Constants exposing (const)
import Dict exposing (Dict)
import Time exposing (Time, inSeconds, now)
import Random exposing (int, step, initialSeed)


type alias Board =
    Dict AxialCoord HexModel


type alias Route =
    Dict AxialCoord HexModel


generateBoard : Time -> ( Int, Int ) -> Board
generateBoard timeAsSeed ( rows, columns ) =
    let
        fishList =
            randomizeFish (timeInSeconds timeAsSeed) (rows * columns)

        mapkeys =
            generateMapKeys rows columns
    in
        List.map2
            (\k v ->
                ( k
                , { emptyHexagon
                    | value = v
                    , center = (axialCoordsToPixel const.hexSize k)
                  }
                )
            )
            mapkeys
            fishList
            |> Dict.fromList


deleteRouteFromBoard : Board -> List AxialCoord -> Board
deleteRouteFromBoard board keyList =
    Dict.filter (\k v -> not (List.member k keyList)) board


findRouteOnBoard : Board -> List AxialCoord -> Route
findRouteOnBoard board keyList =
    Dict.filter (\k v -> List.member k keyList) board


scoreRoute : Route -> Int
scoreRoute route =
    List.map (\tile -> tile.value) (Dict.values route)
        |> List.foldr (+) 0


generateMapKeys : Int -> Int -> List AxialCoord
generateMapKeys maxColumns maxRows =
    List.map convertFromEvenQToAxial (generateAllMapKeys maxColumns 0 maxRows [])


generateAllMapKeys : Int -> Int -> Int -> List AxialCoord -> List AxialCoord
generateAllMapKeys maxColumns currentColumn maxRows list =
    if currentColumn == maxColumns then
        list
    else
        let
            newList =
                list ++ (generateMapKeyListForRow currentColumn maxRows)
        in
            generateAllMapKeys maxColumns (currentColumn + 1) maxRows newList


generateMapKeyListForRow : Int -> Int -> List AxialCoord
generateMapKeyListForRow colNr maxRows =
    List.map (\n -> ( colNr, n )) [0..(maxRows - 1)]


timeInSeconds : Time -> Int
timeInSeconds time =
    round (inSeconds time)


randomizeFish : Int -> Int -> List Int
randomizeFish seed listSize =
    let
        ( nrFish, newSeed ) =
            Random.step (Random.list listSize (Random.int 1 5)) (initialSeed seed)
    in
        nrFish
