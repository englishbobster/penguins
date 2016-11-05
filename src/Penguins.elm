module Penguins exposing (..)

import Hexagon
    exposing
        ( HexModel
        , PixelCoord
        , hexagon
        , emptyHexagon
        )
import Board
    exposing
        ( Board
        , Route
        , generateBoard
        , generateMapKeys
        , findRouteOnBoard
        , deleteRouteFromBoard
        , scoreRoute
        )
import Player
    exposing
        ( PlayerModel
        , Piece
        , PlayerMsg(..)
        , updatePlayer
        , placePlayer
        , drawPlayerPieces
        , getSelectedPiece
        , isPieceSelected
        )
import Model
    exposing
        ( Model
        , GameState(..)
        , updateGameState
        , initialModel
        )
import Helpers
    exposing
        ( AxialCoord
        , convertFromEvenQToAxial
        , axialCoordsToPixel
        , pixelToAxialCoords
        , calculateRoute
        , nearestNeighbours
        )
import Constants exposing (const)
import Dict exposing (Dict, empty, insert, filter)
import Html exposing (Html, div, text, h1)
import Html.App as App
import Svg exposing (Svg)
import Svg.Attributes exposing (height, width)
import String exposing (join)
import Task exposing (perform)
import Mouse exposing (Position, clicks)
import Array exposing (Array)
import Time exposing (Time, now)


--update


type Msg
    = NoOp
    | GenerateBoard Time
    | MousePos Position
    | PlayerMessage PlayerMsg


currentTime : Cmd Msg
currentTime =
    perform (\_ -> NoOp) GenerateBoard now


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
                        ( (movePlayerOne model posAsAxial), Cmd.none )

                    PlayerTwoMove ->
                        ( (movePlayerTwo model posAsAxial), Cmd.none )

                    GameOver ->
                        ( model, Cmd.none )

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
            , board = occupyHexagon coord model.board
          }
            |> updateGameState
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
            , board = occupyHexagon coord model.board
          }
            |> updateGameState
        , Cmd.none
        )
    else
        ( model, Cmd.none )


removePlayerRouteFromBoard : PlayerModel -> AxialCoord -> Board -> Board
removePlayerRouteFromBoard player finalPosition board =
    let
        routeKeyList =
            playerRoute player finalPosition
    in
        deleteRouteFromBoard board routeKeyList


playerRoute : PlayerModel -> AxialCoord -> List AxialCoord
playerRoute player finalPosition =
    let
        selectedPiece =
            getSelectedPiece player

        route =
            calculateRoute selectedPiece.currentPosition finalPosition
    in
        route |> List.take (List.length route - 1)


updatePlayerOne : Model -> Model
updatePlayerOne model =
    { model | playerOne = updateAllowedMovesForOtherPlayer model.playerOne model.board }


movePlayerOne : Model -> AxialCoord -> Model
movePlayerOne model coord =
    let
        routeKeyList =
            playerRoute model.playerOne coord

        route =
            findRouteOnBoard model.board routeKeyList
    in
        if
            (isAllowedMove model.board model.playerOne route coord)
                && (isPieceSelected model.playerOne)
                && (isRouteComplete route routeKeyList)
        then
            { model
                | playerOne = movePiece model.playerOne model.board coord route
                , board =
                    removePlayerRouteFromBoard model.playerOne coord model.board
                        |> occupyHexagon coord
            }
                |> updatePlayerTwo
                |> updateGameState
        else
            model


updatePlayerTwo : Model -> Model
updatePlayerTwo model =
    { model | playerTwo = updateAllowedMovesForOtherPlayer model.playerTwo model.board }


movePlayerTwo : Model -> AxialCoord -> Model
movePlayerTwo model coord =
    let
        routeKeyList =
            playerRoute model.playerTwo coord

        route =
            findRouteOnBoard model.board routeKeyList
    in
        if
            (isAllowedMove model.board model.playerTwo route coord)
                && (isPieceSelected model.playerTwo)
                && (isRouteComplete route routeKeyList)
        then
            { model
                | playerTwo = movePiece model.playerTwo model.board coord route
                , board =
                    removePlayerRouteFromBoard model.playerTwo coord model.board
                        |> occupyHexagon coord
            }
                |> updatePlayerOne
                |> updateGameState
        else
            model


updatePieceForMove : Int -> Board -> AxialCoord -> PlayerModel -> Array Piece
updatePieceForMove index board coord model =
    let
        selectedPiece =
            getSelectedPiece model

        pieceToSet =
            { selectedPiece
                | currentPosition = coord
                , setImage = model.unselectedImage
                , movesAvailable = hasEmptyNeighbourSpaces board coord
            }
    in
        model.placedPieces
            |> updateAllowedMoveForAllPieces board
            |> Array.set index pieceToSet


updateAllowedMovesForOtherPlayer : PlayerModel -> Board -> PlayerModel
updateAllowedMovesForOtherPlayer player board =
    { player | placedPieces = updateAllowedMoveForAllPieces board player.placedPieces }


updateAllowedMoveForAllPieces : Board -> Array Piece -> Array Piece
updateAllowedMoveForAllPieces board pieces =
    Array.map (\piece -> { piece | movesAvailable = hasEmptyNeighbourSpaces board piece.currentPosition }) pieces


movePiece : PlayerModel -> Board -> AxialCoord -> Route -> PlayerModel
movePiece player board coord route =
    let
        points =
            scoreRoute route

        index =
            Maybe.withDefault 4 player.indexSelected
    in
        { player
            | placedPieces =
                updatePieceForMove index board coord player
            , indexSelected = Nothing
            , score = player.score + points
        }


isAllowedMove : Board -> PlayerModel -> Route -> AxialCoord -> Bool
isAllowedMove board player route newPos =
    let
        selectedPiece =
            getSelectedPiece player

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
            && (oldx == newx || oldy == newy || oldz == newz)
            && not (isRouteOccupied route selectedPiece.currentPosition)


hasEmptyNeighbourSpaces : Board -> AxialCoord -> Bool
hasEmptyNeighbourSpaces board coord =
    nearestNeighbours coord
        |> List.any
            (\coord -> isHexagon board coord && not (isOccupiedHexagon board coord))


isRouteComplete : Route -> List AxialCoord -> Bool
isRouteComplete route routeKeyList =
    if (Dict.size route == List.length routeKeyList) then
        True
    else
        False


isRouteOccupied : Route -> AxialCoord -> Bool
isRouteOccupied route startCoord =
    Dict.remove startCoord route
        |> Dict.values
        |> List.any (\tile -> tile.occupied == True)


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


occupyHexagon : AxialCoord -> Board -> Board
occupyHexagon coord board =
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



-- View


view : Model -> Html Msg
view model =
    let
        ( rows, columns ) =
            const.boardSize

        calculatedHeight =
            toString (2 * const.hexSize * rows)
    in
        div []
            [ Svg.svg
                [ height calculatedHeight, width "100%" ]
                ((viewBoard model.board)
                    ++ (viewPlayerPieces model.playerOne)
                    ++ (viewPlayerPieces model.playerTwo)
                )
            , div []
                [ h1 []
                    [ text
                        ("Score PlayerOne: "
                            ++ (toString model.playerOne.score)
                            ++ "          "
                            ++ "Score PlayerTwo: "
                            ++ (toString model.playerTwo.score)
                        )
                    ]
                , h1 [] [ text (toString model.gameState) ]
                ]
            ]


viewPlayerPieces : PlayerModel -> List (Svg Msg)
viewPlayerPieces player =
    List.map
        (\svgmsg ->
            App.map PlayerMessage (svgmsg)
        )
        (drawPlayerPieces player)


viewBoard : Board -> List (Svg Msg)
viewBoard board =
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
