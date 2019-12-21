module Main exposing (..)

import Html exposing (div, text, p)
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- MODEL


type alias JeopardyAnswer =
    { file : Int
    , rank : Int
    }


type alias Model =
    { answers : List JeopardyAnswer
    }


ranksAndFiles : List ( Int, Int )
ranksAndFiles =
    List.concatMap (\n -> List.map ((,) n) <| List.range 1 5) <|
        List.range 1 6


initModel : Model
initModel =
    { answers = List.map (\( x, y ) -> JeopardyAnswer x y) ranksAndFiles }



-- UPDATE


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model

        Decrement ->
            model



-- VIEW

makeRectangle : JeopardyAnswer -> Svg msg
makeRectangle answer =
  rect [ x (toString (200 * answer.file))
        , y (toString (150 * answer.rank))
        , width "200"
        , height "150"
        , fill "blue"
        , stroke "black"
        , strokeWidth "3"
        ] []


gameBoard : Model -> Html.Html msg
gameBoard model =
  svg
    [ viewBox "0 0 2000 1200"
    , width "2000"
    , height "1200"
    ] 
    (List.map makeRectangle model.answers)


view model =
    div []
        [ (gameBoard model)
        , p [] []
        , Html.text (toString model)
        ]


-- MAIN


main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
