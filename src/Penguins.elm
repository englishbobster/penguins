module Penguins exposing (..)

import Dict exposing (Dict, empty, insert)
import Hexagon exposing (drawHexagon)
import Html exposing (Html, text, div)
import Html.App as App
import String exposing (join)
import Time exposing (Time, inSeconds, now)
import Task exposing (perform)
import Random exposing (int, step, initialSeed)


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
    , flipped = False
    , occupied = False
    }


type alias Model =
    HexTile


initialModel : Model
initialModel =
    emptyTile


type Msg
    = NoOp
    | SeedWith Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SeedWith now ->
            ( { model | fish = getFirstFromRandomList (timeInSeconds now) }, Cmd.none )


getFirstFromRandomList : Int -> Int
getFirstFromRandomList seed =
    randomizeFish seed 100
        |> List.head
        |> Maybe.withDefault 1


timeInSeconds : Time -> Int
timeInSeconds time =
    round (inSeconds time)


currentTime : Cmd Msg
currentTime =
    perform (\_ -> NoOp) SeedWith now


randomizeFish : Int -> Int -> List Int
randomizeFish seed sizeOfList =
    let
        ( nrFish, newSeed ) =
            Random.step (Random.list sizeOfList (Random.int 1 5)) (initialSeed seed)
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


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text mapKeysAsText ]
        , div [] [ text (tileAsText model) ]
        ]



--    drawHexagon ( 60, 60 ) 50 "cyan" "yellow"


main =
    App.program
        { init = ( initialModel, currentTime )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }



--test code start


mapKeysAsText : String
mapKeysAsText =
    generateMapKeys 6 6
        |> List.map (\( x, y ) -> "(" ++ (toString x) ++ "," ++ (toString y) ++ ")")
        |> join " ,"


tileAsText : HexTile -> String
tileAsText tile =
    toString tile



--test code end
