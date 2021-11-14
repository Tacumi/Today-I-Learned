module Debugging exposing (main)

import Html exposing (Html, text)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (required)

type Breed
    = Sheltie
    | Poodle
    | Beagle

breedToString : Breed -> String
breedToString breed =
    case breed of
        Sheltie ->
            "Sheltie"
        Poodle ->
            "Poodle"
        Beagle ->
            "Beagle"

type alias Dog =
    { name : String
    , age : Int
    , breed : Breed
    }

decodeBreed : String -> Json.Decoder Breed
decodeBreed breed =
    case breed of
        "Sheltie" ->
            Json.succeed Sheltie
        "Poodle" ->
            Json.succeed Poodle
        "Beagle" ->
            Json.succeed Beagle
        _ ->
            Json.fail ("Unknown breed " ++ breed)
dogDecoder : Json.Decoder Dog
dogDecoder =
    Json.succeed Dog
        |> required "name" Json.string
        |> required "age" Json.int
        |> required "breed" (Json.string |> Json.andThen decodeBreed)

jsonDog : String
jsonDog =
    """
    {
        "name" : "Tucker",
        "age" : 11,
        "breed" : "Poodle"
    }
    """

decodedDog : Result Json.Error Dog
decodedDog =
    Json.decodeString dogDecoder jsonDog

viewDog : Dog -> Html msg
viewDog dog =
    text <|
        dog.name
         p   ++ " the "
            ++ breedToString dog.breed
            ++ " is "
            ++ String.fromInt dog.age
            ++ " years old."

main : Html msg
main =
    case decodedDog of
        Ok dog ->
            viewDog dog
        Err error ->
            Html.pre []
                [text (Json.errorToString error) ]
