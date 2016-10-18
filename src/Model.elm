module Model
    exposing
        ( Model
        , AxialCoord
        , Board
        , Player
        , PlayerState(..)
        , initialModel
        , emptyTile
        , const
        )

import Dict exposing (Dict)
import Hexagon exposing (HexModel)


--Model


type alias Constants =
    { boardSize : ( Int, Int )
    , playerSvgOffset : Float
    , hexSize : Int
    , hexColour : String
    , hexShrinkFactor : Int
    }


const : Constants
const =
    { boardSize = ( 10, 10 )
    , playerSvgOffset = 25
    , hexSize = 55
    , hexColour = "blue"
    , hexShrinkFactor = 5
    }


type alias AxialCoord =
    ( Int, Int )


type alias Board =
    Dict AxialCoord HexModel


type PlayerState
    = NotOnBoard
    | Placed Player


type alias Player =
    { lastPosition : Maybe AxialCoord
    , currentPosition : AxialCoord
    , score : Int
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


type alias Model =
    { board : Board
    , playerState : PlayerState
    }


initialModel : Model
initialModel =
    { board = Dict.empty
    , playerState = NotOnBoard
    }
