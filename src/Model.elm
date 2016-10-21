module Model
    exposing
        ( Model
        , Board
        , initialModel
        , emptyTile
        )

import Dict exposing (Dict)
import Hexagon exposing (HexModel)
import Player exposing (PlayerModel)
import Helpers exposing (AxialCoord)
import Constants exposing (const)


--Model


type alias Board =
    Dict AxialCoord HexModel


type alias Model =
    { board : Board
    , playerOne : PlayerModel
    , playerTwo : PlayerModel
    , position :
        { x : Int
        , y : Int
        }
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
    , position = { x = 0, y = 0 }
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
