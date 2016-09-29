module Penguins exposing (..)

import Dict exposing (Dict, empty, insert)
import Hexagon exposing (drawHexagon, hexagonFace)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Html.App as App
import Svg exposing (Svg, polygon)
import Svg.Attributes exposing (points)
import String exposing (join)
import Time exposing (Time, inSeconds, now)
import Task exposing (perform)
import Random exposing (int, step, initialSeed)


type alias AxialCoords =
    ( Int, Int )


type alias Point =
    ( Float, Float )


type alias HexTile =
    { fish : Int
    , flipped : Bool
    , occupied : Bool
    }


type alias Map =
    Dict AxialCoords HexTile


emptyTile : HexTile
emptyTile =
    { fish = 0
    , flipped = False
    , occupied = False
    }


type alias Model =
    Map


initialModel : Model
initialModel =
    Dict.empty


type Msg
    = NoOp
    | GenerateBoard Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GenerateBoard now ->
            ( (generateBoard now 7 7), Cmd.none )


generateBoard : Time -> Int -> Int -> Model
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
axialHexToPixel size ( r, q ) =
    let
        floatSize =
            toFloat size

        offset =
            floatSize * 6

        x =
            floatSize * (3 / 2) * (toFloat q)

        y =
            floatSize * (sqrt 3) * ((toFloat r) + ((toFloat q) / 2))
    in
        ( offset + x, offset + y )


view : Model -> Html msg
view model =
    let
        svgStyle =
            style
                [ ( "height", "1000px" )
                , ( "width", "100%" )
                ]
    in
        div
            []
            [ Svg.svg [ svgStyle ] (createBoard model) ]


createBoard : Model -> List (Html msg)
createBoard model =
    List.map (\key -> hexagonFace (axialHexToPixel 50 key) 50 "blue") (Dict.keys model)


main =
    App.program
        { init = ( initialModel, currentTime )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }



--test code start


modelAsText : Model -> String
modelAsText model =
    List.map2 (\k v -> ( k, v )) (Dict.keys model) (Dict.values model) |> toString



--test code end
