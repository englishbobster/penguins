module Model
    exposing
        ( Model
        , GameState(..)
        , updateGameState
        , initialModel
        )

import Dict exposing (Dict)
import Array exposing (empty)
import Hexagon exposing (HexModel)
import Board exposing (Board)
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
        , score = 0
        , unselectedImage = const.playerOneUnselectedImage
        , selectedImage = const.playerOneSelectedImage
        }
    , playerTwo =
        { placedPieces = Array.empty
        , indexSelected = Nothing
        , score = 0
        , unselectedImage = const.playerTwoUnselectedImage
        , selectedImage = const.playerTwoSelectedImage
        }
    , gameState = PlayerOnePlacePiece
    }


updateGameState : Model -> Model
updateGameState model =
    if model.gameState == PlayerOnePlacePiece then
        { model | gameState = PlayerTwoPlacePiece }
    else if model.gameState == PlayerTwoPlacePiece then
        if ((Array.length model.playerOne.placedPieces) == const.piecesPerPlayer) then
            { model | gameState = PlayerOneMove }
        else
            { model | gameState = PlayerOnePlacePiece }
    else if model.gameState == PlayerOneMove then
        { model | gameState = PlayerTwoMove }
    else if model.gameState == PlayerTwoMove then
        { model | gameState = PlayerOneMove }
    else
        model
