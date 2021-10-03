module Main exposing (main)
import Html exposing (text)

sayHello : String -> String
sayHello name =
    "Hello, " ++ name ++ "."
greeting : String
greeting =
    sayHello "Elm"

bottlesOf : String -> Int -> String
bottlesOf contents amount = 
    Debug.toString amount ++ " bottles of " ++ contents ++ " on the wall."
main : Html.Html msg
main =
    text (bottlesOf "Juice" 99)