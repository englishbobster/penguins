module Player exposing (PlayerModel, updatePlayer)

import Helpers exposing (AxialCoord, axialCoordsToPixel)
import Constants exposing (const)
import Svg exposing (Svg)
import Svg.Attributes exposing (x, y, height, width, xlinkHref)


type alias PlayerModel =
    { placedPieces :
        List
            { lastPosition : Maybe AxialCoord
            , currentPosition : AxialCoord
            , selected : Bool
            }
    , score : Int
    , image : String
    }


type PlayerMsg
    = PlacePiece AxialCoord
    | Selected


updatePlayer : PlayerMsg -> PlayerModel -> PlayerModel
updatePlayer msg model =
    case msg of
        PlacePiece coords ->
            if (List.length model.placedPieces <= const.piecesPerPlayer) then
                { model
                    | placedPieces =
                        { lastPosition = Nothing
                        , currentPosition = coords
                        , selected = False
                        }
                            :: model.placedPieces
                }
            else
                model

        Selected ->
            model


placePlayer : AxialCoord -> String -> List (Svg msg)
placePlayer coords image =
    let
        ( px, py ) =
            axialCoordsToPixel const.hexSize coords

        xpos =
            x (toString (px - const.playerSvgOffset))

        ypos =
            y (toString (py - const.playerSvgOffset))
    in
        [ Svg.image [ xpos, ypos, height "50", width "50", xlinkHref image ] [] ]
