module Main exposing (..)

import Browser
import Html exposing (div, text, p)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String exposing (fromInt)


-- MODEL

type AnswerStatus = Unread | Correct | Incorrect

type alias JeopardyAnswer =
    { id : Int
    , status : AnswerStatus
    }

type alias Model = List JeopardyAnswer


-- initModel : Model
initModel =
    List.map (\x -> JeopardyAnswer x Unread) (List.range 1 30)

-- UPDATE

update msg model =
  model

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
    Correct -> "green"
    Incorrect -> "red"

makeRectangle : JeopardyAnswer -> Svg msg
makeRectangle answer =
  rect [ x (fromInt (getXCoord answer.id))
        , y (fromInt (getYCoord answer.id))
        , width "140"
        , height "100"
        , fill (getColor answer.status)
        , stroke "black"
        , strokeWidth "2"
        ] []


gameBoard : Model -> Html.Html msg
gameBoard model =
  svg
    [ viewBox "0 0 2000 1200"
    , width "2000"
    , height "1200"
    ] 
    (List.map makeRectangle model)


view model =
    div []
        [ (gameBoard model)
        , p [] []
        ]


-- MAIN


main =
    Browser.sandbox
        { init = initModel
        , update = update
        , view = view
        }
