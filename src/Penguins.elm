module Penguins exposing (..)

import Hexagon exposing (HexModel, Coord, hexagonFace)
import Player exposing (PlayerModel, updatePlayer)
import Model
    exposing
        ( Model
        , Board
        , initialModel
        , emptyTile
        )
import Helpers
    exposing
        ( AxialCoord
        , convertFromEvenQToAxial
        , axialCoordsToPixel
        , pixelToAxialCoords
        )
import Constants exposing (const)
import Dict exposing (Dict, empty, insert)
import Html exposing (Html, div)
import Html.App as App
import Svg exposing (Svg)
import Svg.Attributes exposing (height, width)
import String exposing (join)
import Time exposing (Time, inSeconds, now)
import Task exposing (perform)
import Random exposing (int, step, initialSeed)
import Mouse exposing (Position, clicks)


--update


type Msg
    = NoOp
    | GenerateBoard Time
    | MousePos Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GenerateBoard now ->
            ( { model | board = (generateBoard now const.boardSize) }, Cmd.none )

        MousePos pos ->
            let
                posAsAxial =
                    pixelToAxialCoords const.hexSize
                        ( toFloat pos.x
                        , toFloat pos.y
                        )
            in
                ( model, Cmd.none )


isAllowedMove : Board -> PlayerModel -> AxialCoord -> Bool
isAllowedMove board playermodel newPos =
    let
        ( oldx, oldy ) =
            ( 0, 0 )

        ( newx, newy ) =
            newPos

        oldz =
            0 - oldx - oldy

        newz =
            0 - newx - newy
    in
        True == isTile board newPos && (oldx == newx || oldy == newy || oldz == newz)


isTile : Board -> AxialCoord -> Bool
isTile board coord =
    let
        coordList =
            Dict.keys board
    in
        List.member coord coordList


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
                , { emptyTile
                    | value = v
                    , center = (axialCoordsToPixel const.hexSize k)
                  }
                )
            )
            mapkeys
            fishList
            |> Dict.fromList


timeInSeconds : Time -> Int
timeInSeconds time =
    round (inSeconds time)


currentTime : Cmd Msg
currentTime =
    perform (\_ -> NoOp) GenerateBoard now


randomizeFish : Int -> Int -> List Int
randomizeFish seed listSize =
    let
        ( nrFish, newSeed ) =
            Random.step (Random.list listSize (Random.int 1 5)) (initialSeed seed)
    in
        nrFish


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



-- View


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg
            [ height "1100", width "100%" ]
            ((drawBoard model.board))
        ]


drawBoard : Board -> List (Svg Msg)
drawBoard board =
    let
        getTile : AxialCoord -> HexModel
        getTile tileCoord =
            Dict.get tileCoord board
                |> Maybe.withDefault emptyTile
    in
        List.map (\key -> drawHexagon (getTile key)) (Dict.keys board)


drawHexagon : HexModel -> Svg Msg
drawHexagon tile =
    hexagonFace tile



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    clicks MousePos



-- Main


main =
    App.program
        { init = ( initialModel, currentTime )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
