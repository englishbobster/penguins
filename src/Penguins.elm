module Penguins exposing (..)

import Dict exposing (Dict, empty, insert)
import Hexagon exposing (drawHexagon, hexagonFace)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Html.App as App
import Svg exposing (Svg, polygon, image)
import Svg.Attributes exposing (points, viewBox, x, y, height, width, preserveAspectRatio, xlinkHref)
import String exposing (join)
import Time exposing (Time, inSeconds, now)
import Task exposing (perform)
import Random exposing (int, step, initialSeed)
import Mouse exposing (Position, clicks)


--Model


type alias AxialCoords =
    ( Int, Int )


type alias Point =
    ( Float, Float )


type alias HexTile =
    { fish : Int
    , flipped : Bool
    , occupied : Bool
    }


type alias Board =
    Dict AxialCoords HexTile


type PlayerState
    = NotOnBoard
    | Placed Player


type alias Player =
    { currentPosition : AxialCoords
    , score : Int
    }


emptyTile : HexTile
emptyTile =
    { fish = 0
    , flipped = False
    , occupied = False
    }


type alias Model =
    { board : Board
    , playerState : PlayerState
    , postest : Position
    }


initialModel : Model
initialModel =
    { board = Dict.empty
    , playerState = NotOnBoard
    , postest =
        { x = 0
        , y = 0
        }
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
            ( { model | board = (generateBoard now 10 10) }, Cmd.none )

        MousePos pos ->
            ( { model | postest = pos }, Cmd.none )


generateBoard : Time -> Int -> Int -> Board
generateBoard timeAsSeed rows columns =
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


axialHexToPixel : Int -> AxialCoords -> Point
axialHexToPixel size ( q, r ) =
    let
        floatSize =
            toFloat size

        offset =
            floatSize * 2

        x =
            floatSize * (3 / 2) * (toFloat q)

        y =
            floatSize * (sqrt 3) * ((toFloat r) + ((toFloat q) / 2))
    in
        ( offset + x, offset + y )



-- View


view : Model -> Html msg
view model =
    div []
        [ div []
            [ text (toString model.postest) ]
        , Svg.svg
            [ height "1000", width "100%" ]
            ((drawBoard model.board) ++ [ Svg.image (placePlayer model.postest) [] ])
        ]


placePlayer : Position -> List (Svg.Attribute msg)
placePlayer pos =
    let
        xpos =
            x (toString (pos.x - 25))

        ypos =
            y (toString (pos.y - 25))
    in
        [ xpos, ypos, height "50", width "50", xlinkHref "../graphics/pengmaru.svg" ]


drawBoard : Board -> List (Svg msg)
drawBoard board =
    List.map (\key -> hexagonFace (axialHexToPixel 50 key) 50 "blue" (fishOnTile key board)) (Dict.keys board)


fishOnTile : AxialCoords -> Board -> Int
fishOnTile key board =
    let
        tile =
            Dict.get key board
                |> Maybe.withDefault emptyTile
    in
        tile.fish



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
