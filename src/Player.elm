module Player exposing (PlayerModel, PlayerState(..), placePlayer)

import Helpers exposing (AxialCoord, axialCoordsToPixel)
import Constants exposing (const)
import Svg
import Svg.Attributes exposing (x, y, height, width, xlinkHref)


type PlayerState
    = NoPiecesPlaced
    | Placed PlayerModel


type alias PlayerModel =
    { lastPosition : Maybe AxialCoord
    , currentPosition : AxialCoord
    , score : Int
    , image : String
    }


placePlayer : AxialCoord -> String -> List (Svg.Attribute msg)
placePlayer coords image =
    let
        ( px, py ) =
            axialCoordsToPixel const.hexSize coords

        xpos =
            x (toString (px - const.playerSvgOffset))

        ypos =
            y (toString (py - const.playerSvgOffset))
    in
        [ xpos, ypos, height "50", width "50", xlinkHref image ]
