module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (br, button, div, p, text, h3, a)
import Html.Attributes exposing (style, href)
import Html.Events exposing (onClick)
import Markdown exposing (toHtml)
import String exposing (fromInt)



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

        SetIncorrect n ->            Dict.update n (Maybe.map (\x -> Incorrect)) model

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
        [ Html.Attributes.style "width" "140px"
        , Html.Attributes.style "height" "100px"
        , Html.Attributes.style "margin-top" "10px"
        , Html.Attributes.style "padding-top" "5px"
        , Html.Attributes.style "padding-left" "5px"
        , Html.Attributes.style "background-color" (getColor (Tuple.second answer))
        , Html.Attributes.style "border" "2px solid black"
        ]
        [ button [ onClick (SetCorrect (Tuple.first answer)) ] [ Html.text "Yes" ]
        , Html.text " "
        , button [ onClick (SetIncorrect (Tuple.first answer)) ] [ Html.text "No" ]
        , Html.text " "
        , button [ onClick (SetUnread (Tuple.first answer)) ] [ Html.text "Reset" ]
        ]


stats : Dict Int AnswerStatus -> List (Html.Html Msg)
stats model =
    let
        correct =
            Dict.values model |> List.filter (\e -> e == Correct) |> List.length

        incorrect =
            Dict.values model |> List.filter (\e -> e == Incorrect) |> List.length

        unread =
            Dict.values model |> List.filter (\e -> e == Unread) |> List.length
    in
    [ text <| " Correct: " ++ fromInt correct ++ " | Incorrect: " ++ fromInt incorrect ++ " | Unread: " ++ fromInt unread ]


view : Dict Int AnswerStatus -> Html.Html Msg
view model =
    div [ Html.Attributes.style "padding" "10px" ]
        [ h3 [] [text "Jeopardy! Heatmap"]
        , p [Html.Attributes.style "font-size" "12px"] [text "This is a no-frills scoreboard to track an individual's response rate for a Jeopardy! round.  Simply click \"Yes\" for a given answer if you're correct; click \"No\" otherwise.  The color of each square is updated to reflect its response status, and your tally is tracked below as you update.  To reset the entire board, simply refresh the page."]
        , a [href "https://github.com/msszczep/jeopardy-heatmap/"] [text "Source code is here."]
        , p [] (stats model)
        , div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" "auto auto auto auto auto auto"
            , Html.Attributes.style "padding" "10px"
            , Html.Attributes.style "width" "80%"
            ]
            (List.map makeRectangle (Dict.toList model))
        ]



-- MAIN


main =
    Browser.sandbox
        { init = initModel
        , update = update
        , view = view
        }



