module Penguins exposing (..)

import Hexagon exposing (Msg(..), HexModel, Coord, hexagonFace, updateHex)
import Player exposing (PlayerModel, placePlayer)
import Model
    exposing
        ( Model
        , Board
        , PlayerState(..)
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
    | HexagonMsg Hexagon.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GenerateBoard now ->
            ( { model | board = (generateBoard now const.boardSize) }, Cmd.none )

        MousePos pos ->
            ( { model
                | playerOneState = updatePlayer model pos
              }
            , Cmd.none
            )

        HexagonMsg hexmsg ->
            case hexmsg of
                Something center ->
                    let
                        axcoord =
                            pixelToAxialCoords const.hexSize center

                        tileModel =
                            Dict.get axcoord model.board |> Maybe.withDefault emptyTile

                        modifiedBoard =
                            Dict.insert axcoord (updateHex hexmsg tileModel) model.board
                    in
                        ( { model | board = modifiedBoard }, Cmd.none )


updatePlayer : Model -> Position -> PlayerState
updatePlayer model pos =
    case model.playerOneState of
        NoPiecesPlaced ->
            Placed
                { currentPosition =
                    pixelToAxialCoords const.hexSize
                        ( toFloat pos.x
                        , toFloat pos.y
                        )
                , lastPosition = Nothing
                , score = 0
                , image = const.playerOneImage
                }

        Placed player ->
            let
                newPosition =
                    pixelToAxialCoords const.hexSize
                        ( toFloat pos.x
                        , toFloat pos.y
                        )
            in
                if (isAllowedMove model.board player newPosition) then
                    Placed
                        { lastPosition = Just player.currentPosition
                        , currentPosition = newPosition
                        , score = 0
                        , image = player.image
                        }
                else
                    Placed player


isAllowedMove : Board -> PlayerModel -> AxialCoord -> Bool
isAllowedMove board player newPos =
    let
        ( oldx, oldy ) =
            player.currentPosition

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
    let
        playerPosition =
            case model.playerOneState of
                NoPiecesPlaced ->
                    []

                Placed player ->
                    onBoard model player
    in
        div []
            [ Svg.svg
                [ height "1100", width "100%" ]
                ((drawBoard model.board) ++ playerPosition)
            ]


onBoard : Model -> PlayerModel -> List (Svg msg)
onBoard model player =
    let
        lastKnownPos =
            Maybe.withDefault ( 0, 0 ) player.lastPosition

        newPos =
            player.currentPosition
    in
        if (isTile model.board newPos) then
            [ Svg.image
                (placePlayer newPos player.image)
                []
            ]
        else
            [ Svg.image
                (placePlayer lastKnownPos player.image)
                []
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
    App.map HexagonMsg (hexagonFace tile)



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
