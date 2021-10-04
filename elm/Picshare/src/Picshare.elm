module Picshare exposing (main)
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes
    exposing
        (class, disabled, placeholder, src, type_, value )
import Browser

type Msg
    = ToggleLike
    | UpdateComment String
    | SaveComment

type alias Model =
    { url : String
    , caption : String
    , liked : Bool
    , comments : List String
    , newComment : String
    }

viewComment : String -> Html Msg
viewComment comment =
    li []
        [ strong [] [ text "Comment:" ]
        , text (" " ++ comment)
        ]

viewCommentsList : List String -> Html Msg
viewCommentsList comments =
    case comments of
        [] ->
            text ""
        _ ->
            div [ class "comments" ]
            [ ul []
                (List.map viewComment comments)
            ]

viewComments : Model -> Html Msg
viewComments model =
    div []
        [ viewCommentsList model.comments
        , form [ class "new-comment", onSubmit SaveComment ]
            [ input
                [ type_ "text"
                , placeholder "Add a comment..."
                , value model.newComment
                , onInput UpdateComment
                ]
                []
            , button
                [ disabled (String.isEmpty model.newComment)]
                [ text "Save" ]
            ]
        ]
viewLoveButton : Model -> Html Msg
viewLoveButton model =
    let
        buttonClass =
            if model.liked then
                "fa-heart"
            else
                "fa-heart-o"
    in
    div [ class "like-button" ]
        [ i
            [ class "fa fa-2x"
            , class buttonClass
            , onClick ToggleLike
            ]
            []
        ]
viewDetailedPhoto : Model -> Html Msg
viewDetailedPhoto model =
    div [ class "detailed-photo" ] 
        [ img [ src model.url ] []
        , div [ class "photo-info" ]
            [ viewLoveButton model
            , h2 [ class "caption" ] [ text model.caption ]
            , viewComments model
            ]
        ]

baseUrl : String
baseUrl = 
    "https://programming-elm.com/"

initialModel : Model
initialModel =
    { url = baseUrl ++ "1.jpg"
    , caption = "Surfing"
    , liked = False
    , comments = [ "Cowabunga, dude!" ]
    , newComment = ""
    }

view : Model -> Html Msg
view model =
    div []
    [ div [ class "header"] 
        [ h1 [] [text "Picshare" ] ]
    , div [ class "content-flow" ]
        [ viewDetailedPhoto model ]
    ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleLike ->
            { model | liked = not model.liked }
        UpdateComment comment ->
            { model | newComment = comment }
        SaveComment ->
            saveNewComment model

saveNewComment : Model -> Model
saveNewComment model =
    let
        comment =
            String.trim model.newComment
    in
    case comment of
        "" -> 
            model
        _ ->
            { model
                | comments = model.comments ++ [ comment ]
                , newComment = ""
            }

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }