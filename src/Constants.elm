module Constants exposing (const)


type alias Constants =
    { boardSize : ( Int, Int )
    , playerSvgOffset : Float
    , hexSize : Int
    , hexColour : String
    , hexShrinkFactor : Int
    , playerOneImage : String
    , playerTwoImage : String
    , piecesPerPlayer : Int
    }


const : Constants
const =
    { boardSize = ( 10, 10 )
    , playerSvgOffset = 25
    , hexSize = 55
    , hexColour = "blue"
    , hexShrinkFactor = 5
    , playerOneImage = "../graphics/batzmaru.svg"
    , playerTwoImage = "../graphics/pengmaru.svg"
    , piecesPerPlayer = 3
    }
