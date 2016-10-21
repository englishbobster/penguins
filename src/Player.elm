module Player exposing (PlayerModel, placePlayer, drawPlayerPieces)

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


placePlayer : AxialCoord -> PlayerModel -> PlayerModel
placePlayer coords model =
    if not (allPiecesPlaced model) then
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


drawPlayerPieces : PlayerModel -> List (Svg msg)
drawPlayerPieces model =
    List.map (\piece -> drawPiece piece.currentPosition model.image) model.placedPieces


drawPiece : AxialCoord -> String -> Svg msg
drawPiece coord image =
    let
        ( px, py ) =
            axialCoordsToPixel const.hexSize coord

        xpos =
            x (toString (px - const.playerSvgOffset))

        ypos =
            y (toString (py - const.playerSvgOffset))
    in
        Svg.image [ xpos, ypos, height "50", width "50", xlinkHref image ] []


allPiecesPlaced : PlayerModel -> Bool
allPiecesPlaced model =
    False == (List.length model.placedPieces < const.piecesPerPlayer)
