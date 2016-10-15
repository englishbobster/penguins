module Penguins exposing (..)

import Dict exposing (Dict, empty, insert)
import Hexagon exposing (hexagonFace)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Html.App as App
import Svg exposing (Svg, polygon, image)
import Svg.Attributes
    exposing
        ( points
        , viewBox
        , x
        , y
        , height
        , width
        , preserveAspectRatio
        , xlinkHref
        )
import String exposing (join)
import Time exposing (Time, inSeconds, now)
import Task exposing (perform)
import Random exposing (int, step, initialSeed)
import Mouse exposing (Position, clicks)


--Model


type alias Constants =
    { boardSize : ( Int, Int )
    , playerSvgOffset : Float
    , hexSize : Int
    , hexColour : String
    }


const : Constants
const =
    { boardSize = ( 10, 10 )
    , playerSvgOffset = 25
    , hexSize = 50
    , hexColour = "blue"
    }


type alias AxialCoords =
    ( Int, Int )


type alias Point =
    ( Float, Float )


type alias HexTile =
    { fish : Int }


type alias Board =
    Dict AxialCoords HexTile


type PlayerState
    = NotOnBoard
    | Placed Player


type alias Player =
    { lastPosition : AxialCoords
    , currentPosition : AxialCoords
    , score : Int
    }


emptyTile : HexTile
emptyTile =
    { fish = 0 }


type alias Model =
    { board : Board
    , playerState : PlayerState
    }


initialModel : Model
initialModel =
    { board = Dict.empty
    , playerState = NotOnBoard
    }


type Msg
    = NoOp
    | GenerateBoard Time
    | MousePos Position



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GenerateBoard now ->
            ( { model | board = (generateBoard now const.boardSize) }, Cmd.none )

        MousePos pos ->
            ( { model
                | playerState = updatePlayer model pos
              }
            , Cmd.none
            )


updatePlayer : Model -> Position -> PlayerState
updatePlayer model pos =
    case model.playerState of
        NotOnBoard ->
            Placed
                { currentPosition =
                    pixelToAxialCoords const.hexSize
                        ( toFloat pos.x
                        , toFloat pos.y
                        )
                , lastPosition = ( -100, -100 )
                , score = 0
                }

        Placed player ->
            let
                ( oldx, oldy ) =
                    player.currentPosition

                newPos =
                    pixelToAxialCoords const.hexSize
                        ( toFloat pos.x
                        , toFloat pos.y
                        )

                coordList =
                    Dict.keys model.board
            in
                if List.member newPos coordList then
                    Placed
                        { lastPosition = ( oldx, oldy )
                        , currentPosition = newPos
                        , score = 0
                        }
                else
                    Placed player


generateBoard : Time -> ( Int, Int ) -> Board
generateBoard timeAsSeed ( rows, columns ) =
    let
        fishList =
            randomizeFish (timeInSeconds timeAsSeed) (rows * columns)

        mapkeys =
            generateMapKeys rows columns
    in
        List.map2 (\k v -> ( k, { emptyTile | fish = v } )) mapkeys fishList |> Dict.fromList


timeInSeconds : Time -> Int
timeInSeconds time =
    round (inSeconds time)


currentTime : Cmd Msg
currentTime =
    perform (\_ -> NoOp) GenerateBoard now


randomizeFish : Int -> Int -> List Int
randomizeFish seed listSize =
    let
        ( nrFish, newSeed ) =
            Random.step (Random.list listSize (Random.int 1 5)) (initialSeed seed)
    in
        nrFish


generateMapKeys : Int -> Int -> List AxialCoords
generateMapKeys maxColumns maxRows =
    List.map convertFromEvenQToAxial (generateAllMapKeys maxColumns 0 maxRows [])


generateAllMapKeys : Int -> Int -> Int -> List AxialCoords -> List AxialCoords
generateAllMapKeys maxColumns currentColumn maxRows list =
    if currentColumn == maxColumns then
        list
    else
        let
            newList =
                list ++ (generateMapKeyListForRow currentColumn maxRows)
        in
            generateAllMapKeys maxColumns (currentColumn + 1) maxRows newList


generateMapKeyListForRow : Int -> Int -> List AxialCoords
generateMapKeyListForRow colNr maxRows =
    List.map (\n -> ( colNr, n )) [0..(maxRows - 1)]


convertFromEvenQToAxial : ( Int, Int ) -> AxialCoords
convertFromEvenQToAxial ( col, row ) =
    let
        x =
            col

        z =
            row - (col - (col % 2)) // 2

        y =
            0 - x - z
    in
        ( x, z )


axialCoordsToPixel : Int -> AxialCoords -> Point
axialCoordsToPixel size ( q, r ) =
    let
        offset =
            (toFloat (size * 2))

        x =
            (toFloat size) * (3 / 2) * (toFloat q)

        y =
            (toFloat size) * (sqrt 3) * ((toFloat r) + ((toFloat q) / 2))
    in
        ( offset + x, offset + y )


pixelToAxialCoords : Int -> Point -> AxialCoords
pixelToAxialCoords size ( x, y ) =
    let
        offset =
            (toFloat (size * 2))

        ox =
            x - offset

        oy =
            y - offset

        q =
            (ox * 2 / 3) / (toFloat size)

        r =
            ((-ox / 3) + (((sqrt 3) / 3) * oy)) / (toFloat size)
    in
        ( q, r ) |> roundAxialHex


roundAxialHex : ( Float, Float ) -> AxialCoords
roundAxialHex ( x, y ) =
    let
        z =
            0 - x - y

        rx =
            round x

        ry =
            round y

        rz =
            round (0 - x - y)

        xdiff =
            abs ((toFloat rx) - x)

        ydiff =
            abs ((toFloat ry) - y)

        zdiff =
            abs ((toFloat rz) - z)
    in
        if xdiff > ydiff && xdiff > zdiff then
            ( (0 - ry - rz), ry )
        else if ydiff > zdiff then
            ( rx, (0 - rx - rz) )
        else
            ( rx, ry )



-- View


view : Model -> Html msg
view model =
    let
        playerPosition =
            case model.playerState of
                NotOnBoard ->
                    []

                Placed player ->
                    onBoard model player
    in
        div []
            [ Svg.svg
                [ height "1000", width "100%" ]
                ((drawBoard model.board) ++ playerPosition)
            ]


onBoard : Model -> Player -> List (Svg msg)
onBoard model player =
    let
        coordList =
            Dict.keys model.board
    in
        if List.member player.currentPosition coordList then
            [ Svg.image
                (placePlayer
                    ( (fst player.currentPosition)
                    , (snd player.currentPosition)
                    )
                )
                []
            ]
        else
            [ Svg.image
                (placePlayer
                    ( (fst player.lastPosition)
                    , (snd player.lastPosition)
                    )
                )
                []
            ]


placePlayer : AxialCoords -> List (Svg.Attribute msg)
placePlayer coords =
    let
        ( px, py ) =
            axialCoordsToPixel const.hexSize coords

        xpos =
            x (toString (px - const.playerSvgOffset))

        ypos =
            y (toString (py - const.playerSvgOffset))
    in
        [ xpos, ypos, height "50", width "50", xlinkHref "../graphics/batzmaru.svg" ]


drawBoard : Board -> List (Svg msg)
drawBoard board =
    List.map (\key -> drawHexagon key (fishOnTile key board)) (Dict.keys board)


fishOnTile : AxialCoords -> Board -> String
fishOnTile key board =
    let
        tile =
            Dict.get key board
                |> Maybe.withDefault emptyTile
    in
        toString tile.fish


drawHexagon : AxialCoords -> String -> Svg msg
drawHexagon coord fish =
    hexagonFace (axialCoordsToPixel const.hexSize coord) const.hexSize const.hexColour fish



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    clicks MousePos



-- Main


main =
    App.program
        { init = ( initialModel, currentTime )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--test code start


modelAsText : Model -> String
modelAsText model =
    List.map2 (\k v -> ( k, v )) (Dict.keys model.board) (Dict.values model.board) |> toString



--test code end
