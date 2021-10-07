module Picshare exposing (main, photoDecoder)
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes
    exposing
        (class, disabled, placeholder, src, type_, value, id )
import Browser
import Json.Decode exposing (Decoder, bool, decodeString, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Http
import WebSocket exposing (..)

wsUrl : String
wsUrl = "wss://programming-elm.com/"

type Msg
    = ToggleLike Id
    | UpdateComment Id String
    | SaveComment Id
    | LoadFeed (Result Http.Error Feed)
    | LoadStreamPhoto (Result Json.Decode.Error Photo)
    | FlushStreamQueue

type alias Feed =
    List Photo

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
    { feed : Maybe Feed
    , error : Maybe Http.Error
    , streamQueue : Feed
    }
fetchFeed : Cmd Msg
fetchFeed = 
    Http.get
        { url = baseUrl ++ "feed"
        , expect = Http.expectJson LoadFeed (list photoDecoder)
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

viewContent : Model -> Html Msg
viewContent model =
    case model.error of
        Just error -> 
            div [ class "feed-error" ]
                [ text ( errorMessage error) ]
        Nothing -> 
            viewFeed model.feed

errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadBody _ ->
            """Sorry, we couldn't process your feed at this time.of
               We're working on it!"""
        _ ->
            """Sorry, we couldn't load your feed at this time.of
               Please try again later."""

viewFeed : Maybe Feed -> Html Msg
viewFeed maybeFeed = 
    case maybeFeed of 
        Just feed ->
            div [] (List.map viewDetailedPhoto feed)
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
        , form [ class "new-comment", onSubmit ( SaveComment photo.id )]
            [ input
                [ type_ "text"
                , placeholder "Add a comment..."
                , value photo.newComment
                , onInput ( UpdateComment photo.id )
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
            , onClick (ToggleLike photo.id)
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

updateFeed : ( Photo -> Photo ) -> Id -> Maybe Feed -> Maybe Feed
updateFeed updatePhoto id maybeFeed  =
    Maybe.map (updatePhotoById updatePhoto id) maybeFeed

updatePhotoById : ( Photo -> Photo ) -> Id -> Feed -> Feed
updatePhotoById updatePhoto id feed =
    List.map
        (\photo ->
            if photo.id == id then
                updatePhoto photo
            else
                photo
        )
        feed

viewStreamNotification : Feed -> Html Msg
viewStreamNotification queue =
    case queue of
        [] ->
            text ""
        _ ->
            let
                content = 
                    "View new photos: "
                        ++ String.fromInt (List.length queue)
            in
            div
                [ class "stream-notification"
                , onClick FlushStreamQueue
                ]
                [text content]

baseUrl : String
baseUrl = 
    "https://programming-elm.com/"

initialModel : Model
initialModel =
    { feed = Nothing
    , error = Nothing
    , streamQueue = []
    }

view : Model -> Html Msg
view model =
    div []
    [ div [ class "header"] 
        [ h1 [] [text "Picshare" ] ]
    , div [ class "content-flow" ]
        [ viewStreamNotification model.streamQueue
        , viewContent model
        ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLike id ->
            ( { model 
                | feed = updateFeed toggleLike id model.feed
              }
            , Cmd.none
            )
        UpdateComment id comment ->
            ( { model
                | feed = updateFeed (updateComment comment) id model.feed
              }
            , Cmd.none
            )
        SaveComment id->
            ( { model
                | feed = updateFeed saveNewComment id model.feed
            }
            , Cmd.none
            )
        LoadFeed (Ok feed) ->
            ( { model | feed = Just feed }
            , WebSocket.listen wsUrl
            )
        LoadFeed (Err error) ->
            ( { model | error = Just error } , Cmd.none )
        LoadStreamPhoto (Ok photo) ->
            ( { model | streamQueue = photo :: model.streamQueue }
            , Cmd.none
            )
        LoadStreamPhoto (Err _) ->
            ( model, Cmd.none )
        FlushStreamQueue ->
            ( { model
                | feed = Maybe.map((++) model.streamQueue) model.feed
                , streamQueue = []
                }
            , Cmd.none
            )
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
    WebSocket.receive ( \json -> LoadStreamPhoto ( decodeString photoDecoder json ) )

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