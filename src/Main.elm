module Main exposing (..)

import Browser
import Debug exposing (toString)
import Dict exposing (Dict)
import Html exposing (a, b, br, button, div, h3, input, p, span, table, td, text, th, tr)
import Html.Attributes exposing (class, colspan, href, style, type_)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import Tuple exposing (first, pair, second)



-- MODEL


type AnswerStatus
    = Unread
    | Correct
    | Incorrect


type RoundStatus
    = Jeopardy
    | DoubleJeopardy
    | TripleJeopardy
    | FinalJeopardy


type alias Answer =
    { status : AnswerStatus
    , dailyDouble : Bool
    }


type alias Model =
    { answers : Dict Int Answer
    , round : RoundStatus
    , activatetj : Bool
    }


initModel : Model
initModel =
    Model (Dict.fromList <| List.map (\e -> pair e (Answer Unread False)) <| List.range 1 91) Jeopardy False



-- UPDATE


type Msg
    = SetCorrect Int
    | SetIncorrect Int
    | SetUnread Int
    | SetRound RoundStatus
    | ToggleTripleJeopardy
    | ToggleDailyDouble Int


update : Msg -> Model -> Model
update msg model =
    let
        applyUpdate n a m =
            { model | answers = Dict.update n (Maybe.map (\x -> { x | status = a })) m.answers }
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

        ToggleTripleJeopardy ->
            { model | activatetj = not model.activatetj }

        ToggleDailyDouble n ->
            { model | answers = Dict.update n (Maybe.map (\x -> { x | dailyDouble = not x.dailyDouble })) model.answers }



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


makeRectangle : RoundStatus -> ( Int, Answer ) -> Html.Html Msg
makeRectangle currentRound answer =
    let
        answerData =
            second answer

        ddCheckbox =
            if currentRound == Jeopardy || currentRound == DoubleJeopardy || currentRound == TripleJeopardy then
                [ br [] []
                , button [ onClick (ToggleDailyDouble <| first answer) ] [ text "DD" ]
                ]

            else
                []

        ddLabel =
            if answerData.dailyDouble then
                [ div
                    [ style "font-size" "32px"
                    , style "font-weight" "bold"
                    , style "color"
                        (if answerData.status == Correct then
                            "black"

                         else
                            "white"
                        )
                    ]
                    [ text "DD" ]
                ]

            else
                []
    in
    div
        [ style "width" "140px"
        , style "height" "100px"
        , style "margin-top" "10px"
        , style "padding-top" "5px"
        , style "padding-left" "5px"
        , style "background-color" (getColor answerData.status)
        , style "border" "2px solid black"
        , style "position" "relative"
        ]
        ([ button [ onClick (SetCorrect <| first answer) ] [ text "Yes" ]
         , Html.text " "
         , button [ onClick (SetIncorrect <| first answer) ] [ text "No" ]
         , Html.text " "
         , button [ onClick (SetUnread <| first answer) ] [ text "Reset" ]
         ]
            ++ ddCheckbox
            ++ ddLabel
        )


showRound : Model -> Html.Html Msg
showRound m =
    let
        answersToUse =
            if m.round == Jeopardy then
                List.take 30 (Dict.toList m.answers)

            else if m.round == DoubleJeopardy then
                List.drop 30 (Dict.toList m.answers) |> List.take 30

            else if m.round == TripleJeopardy then
                List.drop 60 (Dict.toList m.answers) |> List.take 30

            else
                List.drop 90 (Dict.toList m.answers)
    in
    div
        [ style "display" "grid"
        , style "grid-template-columns" "auto auto auto auto auto auto"
        , style "padding" "10px"
        ]
        (List.map (makeRectangle m.round) answersToUse)


convertAnswerToEmoji : ( Int, Answer ) -> Html.Html Msg
convertAnswerToEmoji t =
    let
        b =
            second t
    in
    case b.status of
        Unread ->
            text <| "⬛"

        Correct ->
            text <| "🟩"

        Incorrect ->
            text <| "🟥"


makeEmojiHtmlTable answers =
    let
        row1 =
            List.take 6 answers

        row2 =
            List.drop 6 answers |> List.take 6

        row3 =
            List.drop 12 answers |> List.take 6

        row4 =
            List.drop 18 answers |> List.take 6

        row5 =
            List.drop 24 answers
    in
    table []
        [ tr []
            [ td [] (List.map convertAnswerToEmoji row1)
            ]
        , tr []
            [ td [] (List.map convertAnswerToEmoji row2)
            ]
        , tr []
            [ td [] (List.map convertAnswerToEmoji row3)
            ]
        , tr []
            [ td [] (List.map convertAnswerToEmoji row4)
            ]
        , tr []
            [ td [] (List.map convertAnswerToEmoji row5)
            ]
        ]


makeEmojiTableScoreHelper : List ( Int, Answer ) -> String
makeEmojiTableScoreHelper answers =
    answers
        |> List.map second
        |> List.filter (\e -> e.status == Correct)
        |> List.length
        |> String.fromInt


makeEmojiTableScore : List ( Int, Answer ) -> List ( Int, Answer ) -> List ( Int, Answer ) -> ( Int, Answer ) -> Bool -> Html.Html msg
makeEmojiTableScore janswers djanswers tjanswers fjanswer activatetj =
    let
        jc =
            makeEmojiTableScoreHelper janswers

        dc =
            makeEmojiTableScoreHelper djanswers

        tc =
            makeEmojiTableScoreHelper tjanswers

        fc =
            makeEmojiTableScoreHelper [ fjanswer ]

        finalstring =
            if activatetj then
                "( " ++ jc ++ " / " ++ dc ++ " / " ++ tc ++ " / " ++ fc ++ " )"

            else
                "( " ++ jc ++ " / " ++ dc ++ " / " ++ fc ++ " )"
    in
    tr [] [ td [] [ text <| finalstring ] ]


getAnswerCount : Dict Int Answer -> AnswerStatus -> RoundStatus -> String.String
getAnswerCount model a r =
    let
        roundStatusMax =
            if r == Jeopardy then
                31

            else if r == DoubleJeopardy then
                61

            else if r == TripleJeopardy then
                91

            else
                92

        roundStatusMin =
            if r == Jeopardy then
                0

            else if r == DoubleJeopardy then
                30

            else if r == TripleJeopardy then
                60

            else
                90
    in
    model
        |> Dict.toList
        |> List.filter (\e -> first e < roundStatusMax)
        |> List.filter (\e -> first e > roundStatusMin)
        |> List.map second
        |> List.filter (\e -> e.status == a)
        |> List.length
        |> String.fromInt


getNumberStyleList : AnswerStatus -> List (Html.Attribute msg)
getNumberStyleList a =
    [ style "font-size" "150%"
    , style "color" (getColor a)
    , style "text-align" "center"
    ]


showCurrentRoundName : RoundStatus -> Html.Html msg
showCurrentRoundName r =
    span [] []


getVerbiageStyleList : List (Html.Attribute msg)
getVerbiageStyleList =
    [ style "font-weight" "bold"
    , style "padding-top" "10px"
    ]


sumJandDj : String.String -> String.String -> String.String -> String.String -> String.String
sumJandDj j dj tj fj =
    (String.toInt j |> Maybe.withDefault 0)
        + (String.toInt dj |> Maybe.withDefault 0)
        + (String.toInt tj |> Maybe.withDefault 0)
        + (String.toInt fj |> Maybe.withDefault 0)
        |> String.fromInt


newStats : Bool -> Dict Int Answer -> Html.Html Msg
newStats activatetj answers =
    let
        jcorrect =
            getAnswerCount answers Correct Jeopardy

        djcorrect =
            getAnswerCount answers Correct DoubleJeopardy

        tjcorrect =
            getAnswerCount answers Correct TripleJeopardy

        fjcorrect =
            getAnswerCount answers Correct FinalJeopardy

        totalcorrect =
            sumJandDj jcorrect djcorrect tjcorrect fjcorrect

        jwrong =
            getAnswerCount answers Incorrect Jeopardy

        djwrong =
            getAnswerCount answers Incorrect DoubleJeopardy

        tjwrong =
            getAnswerCount answers Incorrect TripleJeopardy

        fjwrong =
            getAnswerCount answers Incorrect FinalJeopardy

        totalwrong =
            sumJandDj jwrong djwrong tjwrong fjwrong

        junread =
            getAnswerCount answers Unread Jeopardy

        djunread =
            getAnswerCount answers Unread DoubleJeopardy

        tjunread =
            getAnswerCount answers Unread TripleJeopardy

        fjunread =
            getAnswerCount answers Unread FinalJeopardy

        totalunread =
            if activatetj == True then
                sumJandDj junread djunread tjunread fjunread

            else
                sumJandDj junread djunread "0" fjunread

        janswers =
            Dict.toList answers |> List.take 30

        djanswers =
            Dict.toList answers |> List.drop 30 |> List.take 30

        tjanswers =
            Dict.toList answers |> List.drop 60 |> List.take 30

        fjanswer =
            Dict.toList answers |> List.drop 90 |> List.head |> Maybe.withDefault ( 0, Answer Unread False )

        statsheaders =
            if activatetj == True then
                [ th [ style "width" "20%" ] [ text "J!" ]
                , th [ style "width" "20%" ] [ text "DJ!" ]
                , th [ style "width" "20%" ] [ text "TJ!" ]
                , th [ style "width" "20%" ] [ text "FJ!" ]
                , th [ style "width" "20%" ] [ text "Total" ]
                ]

            else
                [ th [ style "width" "25%" ] [ text "J!" ]
                , th [ style "width" "25%" ] [ text "DJ!" ]
                , th [ style "width" "25%" ] [ text "FJ!" ]
                , th [ style "width" "25%" ] [ text "Total" ]
                ]

        colspantouse =
            if activatetj == True then
                5

            else
                4

        correctrow =
            if activatetj == True then
                [ td (getNumberStyleList Correct) [ text jcorrect ]
                , td (getNumberStyleList Correct) [ text djcorrect ]
                , td (getNumberStyleList Correct) [ text tjcorrect ]
                , td (getNumberStyleList Correct) [ text fjcorrect ]
                , td (getNumberStyleList Correct) [ text totalcorrect ]
                ]

            else
                [ td (getNumberStyleList Correct) [ text jcorrect ]
                , td (getNumberStyleList Correct) [ text djcorrect ]
                , td (getNumberStyleList Correct) [ text fjcorrect ]
                , td (getNumberStyleList Correct) [ text totalcorrect ]
                ]

        incorrectrow =
            if activatetj == True then
                [ td (getNumberStyleList Incorrect) [ text jwrong ]
                , td (getNumberStyleList Incorrect) [ text djwrong ]
                , td (getNumberStyleList Incorrect) [ text tjwrong ]
                , td (getNumberStyleList Incorrect) [ text fjwrong ]
                , td (getNumberStyleList Incorrect) [ text totalwrong ]
                ]

            else
                [ td (getNumberStyleList Incorrect) [ text jwrong ]
                , td (getNumberStyleList Incorrect) [ text djwrong ]
                , td (getNumberStyleList Incorrect) [ text fjwrong ]
                , td (getNumberStyleList Incorrect) [ text totalwrong ]
                ]

        totalrow =
            if activatetj == True then
                [ td (getNumberStyleList Unread) [ text junread ]
                , td (getNumberStyleList Unread) [ text djunread ]
                , td (getNumberStyleList Unread) [ text tjunread ]
                , td (getNumberStyleList Unread) [ text fjunread ]
                , td (getNumberStyleList Unread) [ text totalunread ]
                ]

            else
                [ td (getNumberStyleList Unread) [ text junread ]
                , td (getNumberStyleList Unread) [ text djunread ]
                , td (getNumberStyleList Unread) [ text fjunread ]
                , td (getNumberStyleList Unread) [ text totalunread ]
                ]

        tjrow =
            if activatetj == True then
                tr [] [ td [ style "padding-top" "20px", colspan colspantouse ] [ makeEmojiHtmlTable tjanswers ] ]

            else
                span [] []
    in
    table
        [ style "text-align" "center"
        , style "width" "100%"
        ]
        [ tr []
            statsheaders
        , tr []
            [ td [ style "padding-top" "10px", colspan colspantouse ] [ text "Correct" ]
            ]
        , tr []
            correctrow
        , tr []
            [ td [ style "padding-top" "10px", colspan colspantouse ] [ text "Incorrect" ]
            ]
        , tr []
            incorrectrow
        , tr []
            [ td [ style "padding-top" "10px", colspan colspantouse ] [ text "Unread" ]
            ]
        , tr []
            totalrow
        , tr []
            [ td [ style "padding-top" "20px", colspan colspantouse ] [ makeEmojiTableScore janswers djanswers tjanswers fjanswer activatetj ] ]
        , tr []
            [ td [ style "padding-top" "20px", colspan colspantouse ] [ makeEmojiHtmlTable janswers ] ]
        , tr []
            [ td [ style "padding-top" "20px", colspan colspantouse ] [ makeEmojiHtmlTable djanswers ] ]
        , tjrow
        , tr []
            [ td [ style "padding-top" "20px" ] [ convertAnswerToEmoji fjanswer ] ]
        ]


view : Model -> Html.Html Msg
view model =
    let
        tjbutton =
            if model.activatetj == True then
                div
                    [ onClick (SetRound TripleJeopardy)
                    , class
                        (if model.round == TripleJeopardy then
                            "active"

                         else
                            ""
                        )
                    , style "display" "inline-block"
                    , style "padding" "10px"
                    , style "margin" "5px"
                    , style "border" "2px solid black"
                    , style "cursor" "pointer"
                    , style "background-color"
                        (if model.round == TripleJeopardy then
                            "#d0d0d0"

                         else
                            "#ffffff"
                        )
                    ]
                    [ text "Triple Jeopardy!" ]

            else
                span [] []
    in
    div [ style "padding" "10px" ]
        [ h3 [] [ text "Jeopardy! Heatmap" ]
        , p [ style "font-size" "14px" ]
            [ text "This is a no-frills scoreboard to track an individual's response rate for a Jeopardy! game.  Simply click \"Yes\" for a given answer if you're correct; click \"No\" otherwise.  The color of each square is updated to reflect its response status, and your tally is tracked below as you update.  The entire game status is also made available as an emoji set, suitable for copying and sharing on social media.  To reset the entire game, simply refresh the page.  "
            , a [ href "https://github.com/msszczep/jeopardy-heatmap/" ] [ text "Source code is here." ]
            ]
        , table []
            [ tr []
                [ td
                    [ style "vertical-align" "top"
                    , style "width" "26%"
                    , style "padding-top" "15px"
                    ]
                    [ div
                        [ style "text-align" "center"
                        ]
                        [ newStats model.activatetj model.answers ]
                    ]
                , td [ style "width" "80%" ]
                    [ div [ style "display" "inline-block" ]
                        [ div
                            [ onClick (SetRound Jeopardy)
                            , class
                                (if model.round == Jeopardy then
                                    "active"

                                 else
                                    ""
                                )
                            , style "display" "inline-block"
                            , style "padding" "10px"
                            , style "margin" "5px"
                            , style "border" "2px solid black"
                            , style "cursor" "pointer"
                            , style "background-color"
                                (if model.round == Jeopardy then
                                    "#d0d0d0"

                                 else
                                    "#ffffff"
                                )
                            ]
                            [ text "Jeopardy!" ]
                        , div
                            [ onClick (SetRound DoubleJeopardy)
                            , class
                                (if model.round == DoubleJeopardy then
                                    "active"

                                 else
                                    ""
                                )
                            , style "display" "inline-block"
                            , style "padding" "10px"
                            , style "margin" "5px"
                            , style "border" "2px solid black"
                            , style "cursor" "pointer"
                            , style "background-color"
                                (if model.round == DoubleJeopardy then
                                    "#d0d0d0"

                                 else
                                    "#ffffff"
                                )
                            ]
                            [ text "Double Jeopardy!" ]
                        , tjbutton
                        , div
                            [ onClick (SetRound FinalJeopardy)
                            , class
                                (if model.round == FinalJeopardy then
                                    "active"

                                 else
                                    ""
                                )
                            , style "display" "inline-block"
                            , style "padding" "10px"
                            , style "margin" "5px"
                            , style "border" "2px solid black"
                            , style "cursor" "pointer"
                            , style "background-color"
                                (if model.round == FinalJeopardy then
                                    "#d0d0d0"

                                 else
                                    "#ffffff"
                                )
                            ]
                            [ text "Final Jeopardy!" ]
                        ]
                    , showRound model
                    , div
                        [ style "padding" "15px"
                        , style "margin-top" "20px"
                        , style "border" "2px solid #0066cc"
                        , style "background-color" "#e6f2ff"
                        , style "border-radius" "8px"
                        ]
                        [ div []
                            [ input [ type_ "checkbox", onClick ToggleTripleJeopardy ] []
                            , text " Enable Triple Jeopardy Round"
                            ]
                        , div [ style "font-size" "12px", style "margin-top" "5px", style "color" "#555" ]
                            [ text "This adds a third full round of 30 questions between Double Jeopardy and Final Jeopardy, expanding the game board." ]
                        ]
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
