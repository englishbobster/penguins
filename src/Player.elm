module Player exposing (PlayerModel, PlayerMsg(..), updatePlayer, placePlayer, drawPlayerPieces, isAnyPieceSelected)

import Helpers exposing (AxialCoord, axialCoordsToPixel)
import Constants exposing (const)
import Svg exposing (Svg)
import Svg.Attributes exposing (x, y, height, width, xlinkHref)
import Svg.Events exposing (onClick)
import Array exposing (Array, push, toList, length, get, toIndexedList)


type alias Piece =
    { lastPosition : Maybe AxialCoord
    , currentPosition : AxialCoord
    , setImage : String
    }


type alias PlayerModel =
    { placedPieces : Array Piece
    , indexSelected : Maybe Int
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
            let
                ( index, piece ) =
                    selectPiece coords model
            in
                { model
                    | placedPieces = updatePieces index piece model
                    , indexSelected = Just index
                }


updatePieces : Int -> Piece -> PlayerModel -> Array Piece
updatePieces index piece model =
    let
        pieceToSet =
            { piece | setImage = model.selectedImage }
    in
        model.placedPieces
            |> Array.map (\piece -> { piece | setImage = model.unselectedImage })
            |> Array.set index pieceToSet


selectPiece : AxialCoord -> PlayerModel -> ( Int, Piece )
selectPiece coords model =
    Array.toIndexedList model.placedPieces
        |> List.filter (\( index, piece ) -> piece.currentPosition == coords)
        |> List.head
        |> Maybe.withDefault
            ( 3
            , { lastPosition = Nothing
              , currentPosition = ( 0, 0 )
              , setImage = " "
              }
            )


isAnyPieceSelected : PlayerModel -> Bool
isAnyPieceSelected model =
    case model.indexSelected of
        Nothing ->
            False

        Just index ->
            True


placePlayer : AxialCoord -> PlayerModel -> PlayerModel
placePlayer coords model =
    let
        newPiece =
            { lastPosition = Nothing
            , currentPosition = coords
            , setImage = model.unselectedImage
            }
    in
        if not (allPiecesPlaced model) then
            { model
                | placedPieces =
                    Array.push newPiece model.placedPieces
            }
        else
            model


drawPlayerPieces : PlayerModel -> List (Svg PlayerMsg)
drawPlayerPieces model =
    Array.toList model.placedPieces
        |> List.map (\piece -> drawPiece piece.currentPosition piece.setImage)


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
    False == (Array.length model.placedPieces < const.piecesPerPlayer)
