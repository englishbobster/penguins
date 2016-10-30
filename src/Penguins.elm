module Penguins exposing (..)

import Hexagon exposing (HexModel, PixelCoord, hexagon)
import Player
    exposing
        ( PlayerModel
        , PlayerMsg(..)
        , updatePlayer
        , placePlayer
        , drawPlayerPieces
        , getSelectedPiece
        , isPieceSelected
        , updatePiecesForMove
        )
import Model
    exposing
        ( Model
        , Board
        , GameState(..)
        , updateGameState
        , initialModel
        , emptyHexagon
        )
import Helpers
    exposing
        ( AxialCoord
        , convertFromEvenQToAxial
        , axialCoordsToPixel
        , pixelToAxialCoords
        , calculateRoute
        )
import Constants exposing (const)
import Dict exposing (Dict, empty, insert)
import Html exposing (Html, div, text)
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
    | PlayerMessage PlayerMsg


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
                case model.gameState of
                    PlayerOnePlacePiece ->
                        placePlayerOne model posAsAxial

                    PlayerTwoPlacePiece ->
                        placePlayerTwo model posAsAxial

                    PlayerOneMove ->
                        movePlayerOne model posAsAxial

                    PlayerTwoMove ->
                        movePlayerTwo model posAsAxial

        PlayerMessage msg ->
            if (model.gameState == PlayerOneMove) then
                ( { model | playerOne = updatePlayer msg model.playerOne }, Cmd.none )
            else if (model.gameState == PlayerTwoMove) then
                ( { model | playerTwo = updatePlayer msg model.playerTwo }, Cmd.none )
            else
                ( model, Cmd.none )


placePlayerOne : Model -> AxialCoord -> ( Model, Cmd Msg )
placePlayerOne model coord =
    if
        (isHexagon model.board coord)
            && not (isOccupiedHexagon model.board coord)
    then
        ( { model
            | playerOne =
                placePlayer coord model.playerOne
            , board = occupyHexagon model.board coord
            , gameState = updateGameState model
          }
        , Cmd.none
        )
    else
        ( model, Cmd.none )


placePlayerTwo : Model -> AxialCoord -> ( Model, Cmd Msg )
placePlayerTwo model coord =
    if
        (isHexagon model.board coord)
            && not (isOccupiedHexagon model.board coord)
    then
        ( { model
            | playerTwo =
                placePlayer coord model.playerTwo
            , board = occupyHexagon model.board coord
            , gameState = updateGameState model
          }
        , Cmd.none
        )
    else
        ( model, Cmd.none )


movePlayerOne : Model -> AxialCoord -> ( Model, Cmd Msg )
movePlayerOne model coord =
    if
        (isAllowedMove model.board model.playerOne coord)
            && (isPieceSelected model.playerOne)
    then
        ( { model
            | playerOne =
                movePiece model.playerOne coord
                --update score as well
            , board =
                occupyHexagon model.board coord
                --must also delete tiles here
            , gameState = updateGameState model
          }
        , Cmd.none
        )
    else
        ( model, Cmd.none )


movePlayerTwo : Model -> AxialCoord -> ( Model, Cmd Msg )
movePlayerTwo model coord =
    if
        (isAllowedMove model.board model.playerTwo coord)
            && (isPieceSelected model.playerTwo)
    then
        ( { model
            | playerTwo =
                movePiece model.playerTwo coord
                --update score as well
            , board =
                occupyHexagon model.board coord
                --must also delete tiles here
            , gameState = updateGameState model
          }
        , Cmd.none
        )
    else
        ( model, Cmd.none )


movePiece : PlayerModel -> AxialCoord -> PlayerModel
movePiece model coord =
    let
        selectedPiece =
            getSelectedPiece model

        index =
            Maybe.withDefault 4 model.indexSelected
    in
        { model
            | placedPieces =
                updatePiecesForMove index selectedPiece coord model
            , indexSelected = Nothing
        }


isAllowedMove : Board -> PlayerModel -> AxialCoord -> Bool
isAllowedMove board playermodel newPos =
    let
        selectedPiece =
            getSelectedPiece playermodel

        ( oldx, oldy ) =
            selectedPiece.currentPosition

        ( newx, newy ) =
            newPos

        oldz =
            0 - oldx - oldy

        newz =
            0 - newx - newy
    in
        True
            == isHexagon board newPos
            && not (isOccupiedHexagon board newPos)
            --must check if path blocked by a piece, opponent or otherwise
            &&
                (oldx == newx || oldy == newy || oldz == newz)


isHexagon : Board -> AxialCoord -> Bool
isHexagon board coord =
    let
        coordList =
            Dict.keys board
    in
        List.member coord coordList


isOccupiedHexagon : Board -> AxialCoord -> Bool
isOccupiedHexagon board coord =
    let
        hexagon =
            Dict.get coord board
    in
        case hexagon of
            Nothing ->
                True

            Just hexagon ->
                hexagon.occupied


occupyHexagon : Board -> AxialCoord -> Board
occupyHexagon board coord =
    let
        occupied : Maybe HexModel -> Maybe HexModel
        occupied maybeHex =
            case maybeHex of
                Nothing ->
                    Just emptyHexagon

                Just maybeHex ->
                    Just { maybeHex | occupied = True }
    in
        Dict.update coord occupied board


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
            ((drawBoard model.board)
                ++ (viewPlayerOnePieces model)
                ++ (viewPlayerTwoPieces model)
            )
        , div [] [ text (toString model.gameState) ]
        , div [] [ text (toString model.playerOne) ]
        , div [] [ text (toString model.playerTwo) ]
        ]


viewPlayerOnePieces : Model -> List (Svg Msg)
viewPlayerOnePieces model =
    List.map
        (\svgmsg ->
            App.map PlayerMessage (svgmsg)
        )
        (drawPlayerPieces model.playerOne)


viewPlayerTwoPieces : Model -> List (Svg Msg)
viewPlayerTwoPieces model =
    List.map
        (\svgmsg ->
            App.map PlayerMessage (svgmsg)
        )
        (drawPlayerPieces model.playerTwo)


drawBoard : Board -> List (Svg Msg)
drawBoard board =
    let
        getHexagon : AxialCoord -> HexModel
        getHexagon tileCoord =
            Dict.get tileCoord board
                |> Maybe.withDefault emptyHexagon
    in
        List.map (\key -> hexagon (getHexagon key)) (Dict.keys board)



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
