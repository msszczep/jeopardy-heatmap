module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (a, br, button, div, h3, p, table, td, text, tr)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import Tuple exposing (first, second)



-- MODEL


type AnswerStatus
    = Unread
    | Correct
    | Incorrect


initModel =
    Dict.fromList <| List.map (\e -> Tuple.pair e Unread) <| List.range 1 30



-- UPDATE


type Msg
    = SetCorrect Int
    | SetIncorrect Int
    | SetUnread Int


update : Msg -> Dict Int AnswerStatus -> Dict Int AnswerStatus
update msg model =
    case msg of
        SetCorrect n ->
            Dict.update n (Maybe.map (\x -> Correct)) model

        SetIncorrect n ->
            Dict.update n (Maybe.map (\x -> Incorrect)) model

        SetUnread n ->
            Dict.update n (Maybe.map (\x -> Unread)) model



-- VIEW


getColor : AnswerStatus -> String
getColor s =
    case s of
        Unread ->
            "blue"

        Correct ->
            "#52D017"

        Incorrect ->
            "red"


makeRectangle : ( Int, AnswerStatus ) -> Html.Html Msg
makeRectangle answer =
    div
        [ style "width" "140px"
        , style "height" "100px"
        , style "margin-top" "10px"
        , style "padding-top" "5px"
        , style "padding-left" "5px"
        , style "background-color" (getColor (second answer))
        , style "border" "2px solid black"
        ]
        [ button [ onClick (SetCorrect (first answer)) ] [ text "Yes" ]
        , Html.text " "
        , button [ onClick (SetIncorrect (first answer)) ] [ text "No" ]
        , Html.text " "
        , button [ onClick (SetUnread (first answer)) ] [ text "Reset" ]
        ]


getAnswerCount : Dict Int AnswerStatus -> AnswerStatus -> Int
getAnswerCount model a =
    Dict.values model |> List.filter (\e -> e == a) |> List.length


getNumberStyleList : AnswerStatus -> List (Html.Attribute msg)
getNumberStyleList a =
    [ style "font-size" "500%"
    , style "color" (getColor a)
    , style "text-align" "center"
    ]


getVerbiageStyleList =
    [ style "font-weight" "bold"
    , style "padding-top" "10px"
    ]


stats : Dict Int AnswerStatus -> List (Html.Html Msg)
stats model =
    let
        correct =
            getAnswerCount model Correct

        incorrect =
            getAnswerCount model Incorrect

        unread =
            getAnswerCount model Unread
    in
    [ div getVerbiageStyleList [ text "Correct:" ]
    , div (getNumberStyleList Correct) [ text <| fromInt correct ]
    , div getVerbiageStyleList [ text "Incorrect:" ]
    , div (getNumberStyleList Incorrect) [ text <| fromInt incorrect ]
    , div getVerbiageStyleList [ text "Unread:" ]
    , div (getNumberStyleList Unread) [ text <| fromInt unread ]
    ]


view : Dict Int AnswerStatus -> Html.Html Msg
view model =
    div [ style "padding" "10px" ]
        [ h3 [] [ text "Jeopardy! Heatmap" ]
        , p [ style "font-size" "14px" ]
            [ text "This is a no-frills scoreboard to track an individual's response rate for a Jeopardy! round.  Simply click \"Yes\" for a given answer if you're correct; click \"No\" otherwise.  The color of each square is updated to reflect its response status, and your tally is tracked below as you update.  To reset the entire board, simply refresh the page.  "
            , a [ href "https://github.com/msszczep/jeopardy-heatmap/" ] [ text "Source code is here." ]
            ]
        , table []
            [ tr []
                [ td
                    [ style "vertical-align" "top"
                    , style "width" "20%"
                    , style "padding-top" "15px"
                    ]
                    [ div [] (stats model) ]
                , td [ style "width" "80%" ]
                    [ div
                        [ style "display" "grid"
                        , style "grid-template-columns" "auto auto auto auto auto auto"
                        , style "padding" "10px"
                        ]
                        (List.map makeRectangle (Dict.toList model))
                    ]
                ]
            ]
        ]



-- MAIN


main =
    Browser.sandbox
        { init = initModel
        , update = update
        , view = view
        }
