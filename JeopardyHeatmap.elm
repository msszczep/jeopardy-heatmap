module Main exposing (..)

import Html exposing (div, text)


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


view model =
    div []
        [ text (toString model)
        ]



-- MAIN


main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
