module Model
    exposing
        ( Model
        , Board
        , initialModel
        , emptyTile
        )

import Dict exposing (Dict)
import Hexagon exposing (HexModel)
import Player exposing (PlayerModel, PlayerState(..))
import Helpers exposing (AxialCoord)
import Constants exposing (const)


--Model


type alias Board =
    Dict AxialCoord HexModel


type GameState
    = SetUp
    | InPlay


type alias Model =
    { board : Board
    , playerOneState : PlayerState
    , playerTwoState : PlayerState
    , gameState : GameState
    }


initialModel : Model
initialModel =
    { board = Dict.empty
    , playerOneState = NoPiecesPlaced
    , playerTwoState = NoPiecesPlaced
    , gameState = InPlay
    }


emptyTile : HexModel
emptyTile =
    { value = 0
    , border = "black"
    , colour = const.hexColour
    , size = const.hexSize
    , center = ( 0, 0 )
    , shrinkFactor = const.hexShrinkFactor
    }
