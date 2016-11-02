module Model
    exposing
        ( Model
        , Board
        , Route
        , GameState(..)
        , updateGameState
        , initialModel
        , emptyHexagon
        )

import Dict exposing (Dict)
import Array exposing (empty)
import Hexagon exposing (HexModel)
import Player exposing (PlayerModel)
import Helpers exposing (AxialCoord)
import Constants exposing (const)


--Model


type GameState
    = PlayerOnePlacePiece
    | PlayerTwoPlacePiece
    | PlayerOneMove
    | PlayerTwoMove
    | GameOver


type alias Board =
    Dict AxialCoord HexModel


type alias Route =
    Dict AxialCoord HexModel


type alias Model =
    { board : Board
    , playerOne : PlayerModel
    , playerTwo : PlayerModel
    , gameState : GameState
    }


initialModel : Model
initialModel =
    { board = Dict.empty
    , playerOne =
        { placedPieces = Array.empty
        , indexSelected = Nothing
        , movesAvailable = True
        , score = 0
        , unselectedImage = const.playerOneUnselectedImage
        , selectedImage = const.playerOneSelectedImage
        }
    , playerTwo =
        { placedPieces = Array.empty
        , indexSelected = Nothing
        , score = 0
        , movesAvailable = True
        , unselectedImage = const.playerTwoUnselectedImage
        , selectedImage = const.playerTwoSelectedImage
        }
    , gameState = PlayerOnePlacePiece
    }


emptyHexagon : HexModel
emptyHexagon =
    { value = 0
    , border = "black"
    , colour = const.hexColour
    , size = const.hexSize
    , center = ( 0, 0 )
    , shrinkFactor = const.hexShrinkFactor
    , occupied = False
    }


updateGameState : Model -> GameState
updateGameState model =
    if model.gameState == PlayerOnePlacePiece then
        PlayerTwoPlacePiece
    else if model.gameState == PlayerTwoPlacePiece then
        if ((Array.length model.playerOne.placedPieces) == const.piecesPerPlayer) then
            PlayerOneMove
        else
            PlayerOnePlacePiece
    else if model.gameState == PlayerOneMove then
        PlayerTwoMove
    else if model.gameState == PlayerTwoMove then
        PlayerOneMove
    else
        model.gameState
