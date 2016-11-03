module Constants exposing (const)


type alias Constants =
    { boardSize : ( Int, Int )
    , playerSvgOffset : Float
    , hexSize : Int
    , hexColour : String
    , hexShrinkFactor : Int
    , playerOneUnselectedImage : String
    , playerTwoUnselectedImage : String
    , playerOneSelectedImage : String
    , playerTwoSelectedImage : String
    , piecesPerPlayer : Int
    }


const : Constants
const =
    { boardSize = ( 3, 3 )
    , playerSvgOffset = 25
    , hexSize = 55
    , hexColour = "purple"
    , hexShrinkFactor = 5
    , playerOneUnselectedImage = "../graphics/batzmaru.svg"
    , playerTwoUnselectedImage = "../graphics/pengmaru.svg"
    , playerOneSelectedImage = "../graphics/batzmaru_selected.svg"
    , playerTwoSelectedImage = "../graphics/pengmaru_selected.svg"
    , piecesPerPlayer = 1
    }
