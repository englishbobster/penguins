module Player exposing (PlayerModel, PlayerMsg(..), updatePlayer, placePlayer, drawPlayerPieces, isAnyPieceSelected)

import Helpers exposing (AxialCoord, axialCoordsToPixel)
import Constants exposing (const)
import Svg exposing (Svg)
import Svg.Attributes exposing (x, y, height, width, xlinkHref)
import Svg.Events exposing (onClick)
import Array exposing (..)


type alias Piece =
    { lastPosition : Maybe AxialCoord
    , currentPosition : AxialCoord
    , selected : Bool
    , setImage : String
    }


type alias PlayerModel =
    { placedPieces : List Piece
    , score : Int
    , unselectedImage : String
    , selectedImage : String
    }


type PlayerMsg
    = Select AxialCoord


updatePlayer : PlayerMsg -> PlayerModel -> PlayerModel
updatePlayer msg model =
    case msg of
        Select coords ->
            { model
                | placedPieces =
                    findAndUpdatePiece coords model.selectedImage model.placedPieces
            }


findAndUpdatePiece : AxialCoord -> String -> List Piece -> List Piece
findAndUpdatePiece coords image pieces =
    let
        setSelected : Piece -> Piece
        setSelected piece =
            if (piece.currentPosition == coords) then
                { piece | selected = True, setImage = image }
            else
                piece
    in
        List.map setSelected pieces


isAnyPieceSelected : PlayerModel -> Bool
isAnyPieceSelected model =
    let
        selectedPieces =
            List.filter (\piece -> piece.selected == True) model.placedPieces
    in
        not (List.isEmpty selectedPieces)


placePlayer : AxialCoord -> PlayerModel -> PlayerModel
placePlayer coords model =
    if not (allPiecesPlaced model) then
        { model
            | placedPieces =
                { lastPosition = Nothing
                , currentPosition = coords
                , selected = False
                , setImage = model.unselectedImage
                }
                    :: model.placedPieces
        }
    else
        model


drawPlayerPieces : PlayerModel -> List (Svg PlayerMsg)
drawPlayerPieces model =
    List.map (\piece -> drawPiece piece.currentPosition piece.setImage) model.placedPieces


drawPiece : AxialCoord -> String -> Svg PlayerMsg
drawPiece coord image =
    let
        ( px, py ) =
            axialCoordsToPixel const.hexSize coord

        xpos =
            x (toString (px - const.playerSvgOffset))

        ypos =
            y (toString (py - const.playerSvgOffset))
    in
        Svg.image
            [ xpos
            , ypos
            , height "50"
            , width "50"
            , xlinkHref image
            , onClick (Select coord)
            ]
            []


allPiecesPlaced : PlayerModel -> Bool
allPiecesPlaced model =
    False == (List.length model.placedPieces < const.piecesPerPlayer)
