module Main exposing (..)

import Browser
import Html exposing (div, text, p, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Dict exposing (Dict)
import String exposing (fromInt)

-- MODEL

type AnswerStatus = Unread | Correct | Incorrect

initModel = Dict.fromList (List.map (\e -> Tuple.pair e Unread) (List.range 1 30))

-- UPDATE

type Msg
  = SetCorrect Int
  | SetIncorrect Int

update : Msg -> Dict Int AnswerStatus -> Dict Int AnswerStatus
update msg model =
  case msg of
    SetCorrect n ->
      Dict.update n (Maybe.map (\x -> Correct)) model
    SetIncorrect n ->
      Dict.update n (Maybe.map (\x -> Incorrect)) model

-- VIEW

getColor : AnswerStatus -> String
getColor s =
  case s of
    Unread -> "blue"
    Correct -> "#52D017"
    Incorrect -> "red"

makeRectangle : (Int, AnswerStatus) -> Html.Html Msg
makeRectangle answer =
  div [ Html.Attributes.style "width" "140px"
      , Html.Attributes.style "height" "100px"
      , Html.Attributes.style "margin-top" "10px"
      , Html.Attributes.style "background-color" (getColor (Tuple.second answer))
      , Html.Attributes.style "border" "2px solid black"
      ]
      [button [ onClick (SetCorrect (Tuple.first answer))] [ Html.text "Yes" ],
      Html.text " ",
      button [ onClick (SetIncorrect (Tuple.first answer))] [ Html.text "No" ]]

view : Dict Int AnswerStatus -> Html.Html Msg
view model =
      div [ Html.Attributes.style "display" "grid"
        , Html.Attributes.style "grid-template-columns" "auto auto auto auto auto auto"
        , Html.Attributes.style "padding" "10px"
        , Html.Attributes.style "width" "80%"
        ]
        (List.map makeRectangle (Dict.toList model))

-- MAIN

main =
    Browser.sandbox
        { init = initModel
        , update = update
        , view = view
        }
