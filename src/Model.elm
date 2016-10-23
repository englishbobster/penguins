module Model
    exposing
        ( Model
        , Board
        , GameState(..)
        , updateGameState
        , initialModel
        , emptyHexagon
        )

import Dict exposing (Dict)
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


type alias Board =
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
        { placedPieces = []
        , score = 0
        , unselectedImage = const.playerOneUnselectedImage
        , selectedImage = const.playerOneSelectedImage
        }
    , playerTwo =
        { placedPieces = []
        , score = 0
        , unselectedImage = const.playerTwoUnselectedImage
        , selectedImage = const.playerOneSelectedImage
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
        if ((List.length model.playerOne.placedPieces) == const.piecesPerPlayer) then
            PlayerOneMove
        else
            PlayerOnePlacePiece
    else if model.gameState == PlayerOneMove then
        PlayerTwoMove
    else if model.gameState == PlayerTwoMove then
        PlayerOneMove
    else
        model.gameState
