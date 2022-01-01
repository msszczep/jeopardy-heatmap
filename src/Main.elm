module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Response
    = Correct
    | Incorrect
    | Unanswered


type alias Model =
    { content : Response }


init : Model
init =
    { content = Unanswered }



-- UPDATE


type Msg
    = SetCorrect
    | SetIncorrect


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetCorrect ->
            { model | content = Correct }

        SetIncorrect ->
            { model | content = Incorrect }



-- VIEW


setColor : Response -> String
setColor response =
    case response of
        Correct ->
            "#12E603"

        Incorrect ->
            "red"

        _ ->
            "blue"


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ viewBox "0 0 200 200"
            , width "200"
            , height "200"
            ]
            [ rect
                [ x "0"
                , y "0"
                , width "100"
                , height "100"
                , fill (setColor model.content)
                , stroke "black"
                , strokeWidth "2"
                ]
                []
            ]
        , button [ onClick SetCorrect ] [ Html.text "Yes" ]
        , button [ onClick SetIncorrect ] [ Html.text "No" ]
        ]
