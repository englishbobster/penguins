module Model
    exposing
        ( Model
        , Board
        , GameState(..)
        , updateGameState
        , initialModel
        , emptyTile
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
    | InPlay


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
        , image = const.playerOneImage
        }
    , playerTwo =
        { placedPieces = []
        , score = 0
        , image = const.playerTwoImage
        }
    , gameState = PlayerOnePlacePiece
    }


emptyTile : HexModel
emptyTile =
    { value = 0
    , border = "black"
    , colour = const.hexColour
    , size = const.hexSize
    , center = ( 0, 0 )
    , shrinkFactor = const.hexShrinkFactor
    , occupied = False
    }


updateGameState : GameState -> GameState
updateGameState gameState =
    if gameState == PlayerOnePlacePiece then
        PlayerTwoPlacePiece
    else if gameState == PlayerTwoPlacePiece then
        PlayerOnePlacePiece
    else
        gameState
