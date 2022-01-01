module Main exposing (..)

import Browser
import Html exposing (div, text, p, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Dict exposing (Dict)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String exposing (fromInt)


-- MODEL

type AnswerStatus = Unread | Correct | Incorrect

initModel = Dict.fromList (List.map (\e -> Tuple.pair e Unread) (List.range 1 6))

-- UPDATE

type Msg
  = SetCorrect Int
  | SetIncorrect Int


update msg model =
  case msg of
    SetCorrect n ->
      Dict.update n (Maybe.map (\x -> Correct)) model
    SetIncorrect n ->
      Dict.update n (Maybe.map (\x -> Incorrect)) model

-- VIEW

getXCoord : Int -> Int
getXCoord n = 
  if modBy 6 n == 1 then 10
  else if modBy 6 n == 2 then 160
  else if modBy 6 n == 3 then 310
  else if modBy 6 n == 4 then 460
  else if modBy 6 n == 5 then 610
  else if modBy 6 n == 0 then 760
  else 1

getYCoord : Int -> Int
getYCoord n = 
  if n < 7 then 
    10
  else if n < 13 && n > 6 then 
    120
  else if n < 19 && n > 12 then
    230
  else if n < 25 && n > 18 then
    340
  else
    450

getColor : AnswerStatus -> String
getColor s =
  case s of
    Unread -> "blue"
    Correct -> "#52D017"
    Incorrect -> "red"

-- makeRectangle : JeopardyAnswer -> Svg msg
makeRectangle answer =
  div [ Html.Attributes.style "margin-left" "10px"
      , Html.Attributes.style "margin-top" (String.concat [(fromInt (getYCoord (Tuple.first answer))), "px"])
      , Html.Attributes.style "float" "left"
      , Html.Attributes.style "width" "140px"
      , Html.Attributes.style "height" "100px"
      , Html.Attributes.style "background-color" (getColor (Tuple.second answer))
      , Html.Attributes.style "border" "2px solid black"
      ]
      [Html.text " ",
       button [ onClick (SetCorrect (Tuple.first answer))] [ Html.text "Yes" ],
      Html.text " ",
      button [ onClick (SetIncorrect (Tuple.first answer))] [ Html.text "No" ]]

--gameBoard : Model -> Html.Html msg
gameBoard model =
  List.map makeRectangle (Dict.toList model)


view model =
    div []
        (gameBoard model)


-- MAIN


main =
    Browser.sandbox
        { init = initModel
        , update = update
        , view = view
        }
