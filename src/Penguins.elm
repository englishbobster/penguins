module Penguins exposing (..)

import Dict exposing (Dict, empty, insert)
import Hexagon exposing (drawHexagon)
import Html exposing (Html, text)
import Html.App as App
import String exposing (join)
import Time exposing (Time, inSeconds, now)
import Task exposing (perform)


type alias AxialCoords =
    ( Int, Int )


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
    , flipped = True
    , occupied = False
    }


type alias Model =
    Map


initialModel : Model
initialModel =
    Dict.empty


type Msg
    = NoOp
    | SeedWith Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SeedWith now ->
            ( model, Cmd.none )


timeInSeconds : Time -> Int
timeInSeconds time =
    round (inSeconds time)


currentTime : Cmd Msg
currentTime =
    perform (\_ -> NoOp) SeedWith now


generateMap : Int -> Int -> Map
generateMap height width =
    insert ( 0, 0 ) (generateTile 0) empty


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


generateTile : Int -> HexTile
generateTile nrFish =
    emptyTile


view : Model -> Html Msg
view model =
    drawHexagon ( 60, 60 ) 50 "cyan" "yellow"


main =
    App.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }



--test code start


mapKeysAsText : String
mapKeysAsText =
    List.map (\( x, y ) -> "(" ++ (toString x) ++ "," ++ (toString y) ++ ")") (generateMapKeys 6 6)
        |> join " ,"



--test code end
