module Main exposing (..)

import Browser
import Debug exposing (toString)
import Dict exposing (Dict)
import Html exposing (a, b, br, button, div, h3, p, table, td, text, th, tr)
import Html.Attributes exposing (colspan, href, style)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import Tuple exposing (first, pair, second)


-- TODO:
-- * Track Daily Doubles
-- * Make copyable results for social media, a la Wordle : red, green and blue square emojis

-- MODEL


type AnswerStatus
    = Unread
    | Correct
    | Incorrect

type RoundStatus
    = Jeopardy
    | DoubleJeopardy
    | FinalJeopardy

type alias Model =
    { answers: Dict Int AnswerStatus
    , round: RoundStatus
    }

initModel : Model
initModel =
    Model (Dict.fromList <| List.map (\e -> pair e Unread) <| List.range 1 61) Jeopardy

-- UPDATE


type Msg
    = SetCorrect Int
    | SetIncorrect Int
    | SetUnread Int
    | SetRound RoundStatus

update : Msg -> Model -> Model
update msg model =
    let
        applyUpdate n a m =
            { model | answers = Dict.update n (Maybe.map (\x -> a)) m.answers }

    in
    case msg of
        SetCorrect n ->
            applyUpdate n Correct model

        SetIncorrect n ->
            applyUpdate n Incorrect model

        SetUnread n ->
            applyUpdate n Unread model

        SetRound r ->
            { model | round = r }

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
        [ button [ onClick (SetCorrect <| first answer) ] [ text "Yes" ]
        , Html.text " "
        , button [ onClick (SetIncorrect <| first answer) ] [ text "No" ]
        , Html.text " "
        , button [ onClick (SetUnread <| first answer) ] [ text "Reset" ]
        ]

showRound : Model -> Html.Html Msg
showRound m =
     let
        answersToUse =
           if m.round == Jeopardy
                     then List.take 30 (Dict.toList m.answers)
           else if m.round == DoubleJeopardy
              then List.drop 30 (Dict.toList m.answers) |> List.take 30
           else
              List.drop 60 (Dict.toList m.answers)
     in
     div
       [ style "display" "grid"
                        , style "grid-template-columns" "auto auto auto auto auto auto"
                        , style "padding" "10px"
                        ]

       (List.map makeRectangle answersToUse)

getAnswerCount : Dict Int AnswerStatus -> AnswerStatus -> RoundStatus -> String.String
getAnswerCount model a r =
    let
        roundStatusMax = if r == Jeopardy then 31 else 61
        roundStatusMin = if r == Jeopardy then 0 else 30
    in
        model
            |> Dict.toList
            |> List.filter (\e -> first e < roundStatusMax)
            |> List.filter (\e -> first e > roundStatusMin)
            |> List.map second
            |> List.filter (\e -> e == a)
            |> List.length
            |> String.fromInt

getRedZoneCorrectCount : Dict Int AnswerStatus -> Int
getRedZoneCorrectCount model =
    model
        |> Dict.toList
        |> List.filter (\e -> first e > 12)
        |> List.map second
        |> List.filter (\e -> e == Correct)
        |> List.length


getNumberStyleList : AnswerStatus -> List (Html.Attribute msg)
getNumberStyleList a =
    [ style "font-size" "250%"
    , style "color" (getColor a)
    , style "text-align" "center"
    ]


showCurrentRoundName : RoundStatus -> Html.Html msg
showCurrentRoundName r =
    let
      t = if r == Jeopardy
              then " - Round: Jeopardy!"
          else if r == DoubleJeopardy
              then " - Round: Double Jeopardy!"
          else
              " - Round: Final Jeopardy!"
    in
      b [] [text t]

getVerbiageStyleList : List (Html.Attribute msg)
getVerbiageStyleList =
    [ style "font-weight" "bold"
    , style "padding-top" "10px"
    ]


sumJandDj : String.String -> String.String -> String.String
sumJandDj j dj =
    (String.toInt j |> Maybe.withDefault 0) +
    (String.toInt dj |> Maybe.withDefault 0)
     |> String.fromInt

newStats : Dict Int AnswerStatus -> Html.Html Msg
newStats answers =
   let
       jcorrect = getAnswerCount answers Correct Jeopardy
       djcorrect = getAnswerCount answers Correct DoubleJeopardy
       totalcorrect = sumJandDj jcorrect djcorrect
       jwrong = getAnswerCount answers Incorrect Jeopardy
       djwrong = getAnswerCount answers Incorrect DoubleJeopardy
       totalwrong = sumJandDj jwrong djwrong
       junread = getAnswerCount answers Unread Jeopardy
       djunread = getAnswerCount answers Unread DoubleJeopardy
       totalunread = sumJandDj junread djunread
   in
       table [
         style "text-align" "center"
         , style "width" "100%"
       ] [
          tr [] [
            th [style "width" "33%"] [text "J!"]
            , th [style "width" "33%"] [text "DJ!"]
            , th [style "width" "33%"] [text "Total"]
          ] ,
          tr [] [
            td [style "padding-top" "30px", colspan 3] [text "Correct"]
          ] ,
          tr [] [
            td (getNumberStyleList Correct) [text jcorrect]
            , td (getNumberStyleList Correct) [text djcorrect]
            , td (getNumberStyleList Correct) [text totalcorrect]
          ] ,
          tr [] [
            td [style "padding-top" "30px", colspan 3] [text "Incorrect"]
          ] ,
          tr [] [
            td (getNumberStyleList Incorrect) [text jwrong]
            , td (getNumberStyleList Incorrect) [text djwrong]
            , td (getNumberStyleList Incorrect) [text totalwrong]
          ] ,
          tr [] [
            td [style "padding-top" "30px", colspan 3] [text "Unread"]
          ] ,
          tr [] [
            td (getNumberStyleList Unread) [text junread]
            , td (getNumberStyleList Unread) [text djunread]
            , td (getNumberStyleList Unread) [text totalunread]
          ]
       ]

view : Model -> Html.Html Msg
view model =
    div [ style "padding" "10px" ]
        [ h3 [] [ text "Jeopardy! Heatmap" ]
        , p [ style "font-size" "14px" ]
            [ text "This is a no-frills scoreboard to track an individual's response rate for a Jeopardy! game.  Simply click \"Yes\" for a given answer if you're correct; click \"No\" otherwise.  The color of each square is updated to reflect its response status, and your tally is tracked below as you update.  To reset the entire game, simply refresh the page.  "
            , a [ href "https://github.com/msszczep/jeopardy-heatmap/" ] [ text "Source code is here." ]
            ]
        , table []
            [ tr []
                [ td
                    [ style "vertical-align" "top"
                    , style "width" "26%"
                    , style "padding-top" "15px"
                    ]
                    [ div [
                      style "text-align" "center"
                    ] [(newStats model.answers)]]
                , td [ style "width" "80%" ]
                    [ button [onClick (SetRound Jeopardy) ] [text "Jeopardy!"]
                    , Html.text " "
                    , button [onClick (SetRound DoubleJeopardy) ] [text "Double Jeopardy!"]
                    , Html.text " "
                    , button [onClick (SetRound FinalJeopardy) ] [text "Final Jeopardy!"]
                    , (showCurrentRoundName model.round)
                    , showRound model
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
