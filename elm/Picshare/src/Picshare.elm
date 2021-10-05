module Picshare exposing (main, photoDecoder)
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes
    exposing
        (class, disabled, placeholder, src, type_, value )
import Browser
import Browser.Dom exposing (Error)
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Http

type Msg
    = ToggleLike
    | UpdateComment String
    | SaveComment
    | LoadFeed (Result Http.Error Photo)

type alias Id =
    Int

type alias Photo =
    { id : Id
    , url : String
    , caption : String
    , liked : Bool
    , comments : List String
    , newComment : String
    }
type alias Model =
    { photo : Maybe Photo
    }
fetchFeed : Cmd Msg
fetchFeed = 
    Http.get
        { url = baseUrl ++ "feed/1"
        , expect = Http.expectJson LoadFeed photoDecoder
        }
photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "id" int
        |> required "url" string
        |> required "caption" string
        |> required "liked" bool
        |> required "comments" (list string)
        |> hardcoded ""

viewFeed : Maybe Photo -> Html Msg
viewFeed maybePhoto = 
    case maybePhoto of 
        Just photo ->
            viewDetailedPhoto photo
        Nothing ->
            div [ class "loading-feed" ]
                [ text "Loading Feed..." ]

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

viewComments : Photo -> Html Msg
viewComments photo =
    div []
        [ viewCommentsList photo.comments
        , form [ class "new-comment", onSubmit SaveComment ]
            [ input
                [ type_ "text"
                , placeholder "Add a comment..."
                , value photo.newComment
                , onInput UpdateComment
                ]
                []
            , button
                [ disabled (String.isEmpty photo.newComment)]
                [ text "Save" ]
            ]
        ]
viewLoveButton : Photo -> Html Msg
viewLoveButton photo =
    let
        buttonClass =
            if photo.liked then
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
viewDetailedPhoto : Photo -> Html Msg
viewDetailedPhoto photo =
    div [ class "detailed-photo" ] 
        [ img [ src photo.url ] []
        , div [ class "photo-info" ]
            [ viewLoveButton photo
            , h2 [ class "caption" ] [ text photo.caption ]
            , viewComments photo
            ]
        ]

toggleLike : Photo -> Photo
toggleLike photo =
    { photo | liked = not photo.liked }

updateComment : String -> Photo -> Photo
updateComment comment photo =
    { photo | newComment = comment }

updateFeed : ( Photo -> Photo ) -> Maybe Photo -> Maybe Photo
updateFeed updatePhoto maybePhoto =
    Maybe.map updatePhoto maybePhoto

baseUrl : String
baseUrl = 
    "https://programming-elm.com/"

initialModel : Model
initialModel =
    { photo = Nothing }

view : Model -> Html Msg
view model =
    div []
    [ div [ class "header"] 
        [ h1 [] [text "Picshare" ] ]
    , div [ class "content-flow" ]
        [ viewFeed model.photo ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLike ->
            ( { model 
                | photo = updateFeed toggleLike model.photo
              }
            , Cmd.none
            )
        UpdateComment comment ->
            ( { model
                | photo = updateFeed (updateComment comment) model.photo
              }
            , Cmd.none
            )
        SaveComment ->
            ( { model
                | photo = updateFeed saveNewComment model.photo
            }
            , Cmd.none
            )
        LoadFeed (Ok photo) ->
            ( { model | photo = Just photo }
            , Cmd.none 
            )
        LoadFeed (Err _) ->
            ( model, Cmd.none )

saveNewComment : Photo -> Photo
saveNewComment photo =
    let
        comment =
            String.trim photo.newComment
    in
    case comment of
        "" -> 
            photo
        _ ->
            { photo
                | comments = photo.comments ++ [ comment ]
                , newComment = ""
            }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

init : () -> ( Model, Cmd Msg )
init () =
    (initialModel, fetchFeed)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }