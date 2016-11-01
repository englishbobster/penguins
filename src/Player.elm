module Player
    exposing
        ( PlayerModel
        , PlayerMsg(..)
        , updatePlayer
        , placePlayer
        , drawPlayerPieces
        , isPieceSelected
        , getSelectedPiece
        , updatePiecesForMove
        )

import Helpers exposing (AxialCoord, axialCoordsToPixel)
import Constants exposing (const)
import Svg exposing (Svg)
import Svg.Attributes exposing (x, y, height, width, xlinkHref)
import Svg.Events exposing (onClick)
import Array exposing (Array, push, toList, length, get, toIndexedList)


type alias Piece =
    { currentPosition : AxialCoord
    , setImage : String
    }


dummyPiece =
    { currentPosition = ( 100, 100 )
    , setImage = ""
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
                    findPieceByCoords coords model
            in
                { model
                    | placedPieces = updatePiecesForSelection index piece model
                    , indexSelected = Just index
                }


updatePiecesForSelection : Int -> Piece -> PlayerModel -> Array Piece
updatePiecesForSelection index piece model =
    let
        pieceToSet =
            { piece | setImage = model.selectedImage }
    in
        model.placedPieces
            |> Array.map (\piece -> { piece | setImage = model.unselectedImage })
            |> Array.set index pieceToSet


updatePiecesForMove : Int -> AxialCoord -> PlayerModel -> Array Piece
updatePiecesForMove index coord model =
    let
        selectedPiece =
            getSelectedPiece model

        pieceToSet =
            { selectedPiece
                | currentPosition = coord
                , setImage = model.unselectedImage
            }
    in
        model.placedPieces
            |> Array.set index pieceToSet


findPieceByCoords : AxialCoord -> PlayerModel -> ( Int, Piece )
findPieceByCoords coords model =
    Array.toIndexedList model.placedPieces
        |> List.filter (\( index, piece ) -> piece.currentPosition == coords)
        |> List.head
        |> Maybe.withDefault ( 3, dummyPiece )


getSelectedPiece : PlayerModel -> Piece
getSelectedPiece model =
    case model.indexSelected of
        Nothing ->
            dummyPiece

        Just index ->
            Array.get index model.placedPieces
                |> Maybe.withDefault dummyPiece


isPieceSelected : PlayerModel -> Bool
isPieceSelected model =
    case model.indexSelected of
        Nothing ->
            False

        Just index ->
            True


placePlayer : AxialCoord -> PlayerModel -> PlayerModel
placePlayer coords model =
    let
        newPiece =
            { currentPosition = coords
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
