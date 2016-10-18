module Player exposing (PlayerModel)

import Helpers exposing (AxialCoord)


type alias PlayerModel =
    { lastPosition : Maybe AxialCoord
    , currentPosition : AxialCoord
    , score : Int
    }
