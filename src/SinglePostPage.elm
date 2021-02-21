module SinglePostPage exposing (..)

import Accessibility as Html exposing (Html, div, h1, img, p, span, text)
import Browser
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Regular as IconReg
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, nonNullElementsOrFail, nonNullOrFail, with)
import Html
import Html.Attributes exposing (class, src)
import Html.Attributes.Aria exposing (ariaHidden)
import Html.Parser exposing (Node)
import Html.Parser.Util
import Iso8601
import RemoteData exposing (RemoteData)
import Taco.Enum.PostObjectFieldFormatEnum exposing (PostObjectFieldFormatEnum)
import Taco.Object exposing (Avatar, NodeWithAuthorToUserConnectionEdge, Post, User)
import Taco.Object.Avatar as Avatar
import Taco.Object.NodeWithAuthorToUserConnectionEdge as NodeWithAuthorToUserConnectionEdge
import Taco.Object.Post as Post
import Taco.Object.User as User
import Taco.Query as Query exposing (postBy)
import Time exposing (Month(..))



---- PROGRAM ----


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



---- INIT ----


init : String -> ( Model, Cmd Msg )
init flags =
    ( { initialModel | slug = flags }, getPost flags )


initialModel : Model
initialModel =
    { slug = "i-sure-love-tacos"
    , post = RemoteData.Loading
    }



---- MODEL ----


type alias Model =
    { slug : String
    , post : RemoteData (Graphql.Http.Error (Maybe Post)) (Maybe Post)
    }


type alias Post =
    { date : Maybe String
    , commentCount : Maybe Int
    , uri : String
    , title : Maybe String
    , content : Maybe String
    , author : Maybe User
    }


type alias User =
    { firstName : Maybe String
    , nickname : Maybe String
    , avatar : Maybe Avatar
    , description : Maybe String
    }


type alias Avatar =
    { url : Maybe String
    , isRestricted : Maybe Bool
    }



---- UPDATE ----


type Msg
    = GotResponse (RemoteData (Graphql.Http.Error (Maybe Post)) (Maybe Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            case response of
                RemoteData.Success data ->
                    ( { model | post = response }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



---- QUERY ----


getPost : String -> Cmd Msg
getPost slug =
    postQuery slug
        |> Graphql.Http.queryRequest "http://localhost/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


postQuery : String -> SelectionSet (Maybe Post) RootQuery
postQuery slug =
    Query.postBy (getPostOptArgs slug) postSelection


getPostOptArgs : String -> Query.PostByOptionalArguments -> Query.PostByOptionalArguments
getPostOptArgs slug args =
    { id = Null
    , postId = Null
    , uri = Null
    , slug = Graphql.OptionalArgument.Present slug
    }


postSelection : SelectionSet Post Taco.Object.Post
postSelection =
    SelectionSet.map6 Post
        Post.date
        Post.commentCount
        Post.uri
        (Post.title postTitleArgs)
        (Post.content postContentArgs)
        (Post.author authorSelectionEdge)


authorSelectionEdge : SelectionSet User Taco.Object.NodeWithAuthorToUserConnectionEdge
authorSelectionEdge =
    NodeWithAuthorToUserConnectionEdge.node authorSelection
        |> nonNullOrFail


authorSelection : SelectionSet User Taco.Object.User
authorSelection =
    SelectionSet.map4 User
        User.firstName
        User.nickname
        (User.avatar avatarOptArgs avatarSelection)
        User.description


avatarSelection : SelectionSet Avatar Taco.Object.Avatar
avatarSelection =
    SelectionSet.map2 Avatar
        Avatar.url
        Avatar.isRestricted


avatarOptArgs : User.AvatarOptionalArguments -> User.AvatarOptionalArguments
avatarOptArgs args =
    { size = Graphql.OptionalArgument.Null
    , forceDefault = Graphql.OptionalArgument.Null
    , rating = Graphql.OptionalArgument.Null
    }


postTitleArgs : Post.TitleOptionalArguments -> Post.TitleOptionalArguments
postTitleArgs args =
    { format = Graphql.OptionalArgument.Null }


postContentArgs : Post.ContentOptionalArguments -> Post.ContentOptionalArguments
postContentArgs args =
    { format = Graphql.OptionalArgument.Present Taco.Enum.PostObjectFieldFormatEnum.Rendered }



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        content =
            case model.post of
                RemoteData.Success data ->
                    case data of
                        Just res ->
                            div [] [ viewPost res ]

                        _ ->
                            div []
                                [ h1 []
                                    [ text "Sorry! This is not the taco you're looking for" ]
                                , text "The blog post you're looking for couldn't be found."
                                ]

                RemoteData.Loading ->
                    div [ class "page-spinner" ]
                        [ viewSpinner
                        , span [ class "sr-only" ] [ text "Loading posts..." ]
                        , span [ ariaHidden True ] [ text " Prepping Tacos" ]
                        ]

                _ ->
                    div []
                        [ h1 []
                            [ text "Sorry! This is not the taco you're looking for" ]
                        , text "The blog post you're looking for couldn't be found."
                        ]
    in
    div [ class "page animate__animated animate__backInUp" ]
        [ div [] [ content ] ]


viewPost : Post -> Html Msg
viewPost post =
    let
        title =
            justOrEmpty post.title

        dateResult =
            Iso8601.toTime (justOrEmpty post.date)

        timeString =
            case dateResult of
                Ok res ->
                    getTime res

                _ ->
                    "Unknown Date"

        postInfo =
            case post.author of
                Just res ->
                    "By " ++ justOrEmpty res.firstName ++ " on " ++ timeString

                _ ->
                    "Posted on " ++ timeString

        author =
            case post.author of
                Just res ->
                    viewAuthor res

                _ ->
                    text ""

        content =
            justOrEmpty post.content
    in
    div [ class "post" ]
        [ h1 [ class "post-title" ] [ text title ]
        , span [ class "post-author" ] [ text postInfo ]
        , div [ class "post-body" ]
            (Html.Parser.Util.toVirtualDom (getParsedHtml content))
        , author
        ]


viewAuthor : User -> Html Msg
viewAuthor author =
    let
        firstName =
            justOrEmpty author.firstName

        descriptionLines =
            String.split "\n" (justOrEmpty author.description)

        avatar =
            case author.avatar of
                Just res ->
                    justOrEmpty res.url

                _ ->
                    ""
    in
    div [ class "post-about" ]
        [ div [ class "about-image" ]
            [ img avatar [ src avatar ] ]
        , div [ class "about-text" ]
            [ span [ class "text-bold text-large stack-m" ] [ text <| firstName ]
            , span [ class "text-italic text-light stack-s" ] [ text "Taco Consumer, Part-time Genius, Taco Developer, Author" ]
            , span [] (List.map (\line -> p [] [ text line ]) descriptionLines)
            ]
        ]


monthToString : Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


getParsedHtml : String -> List Node
getParsedHtml html =
    case Html.Parser.run html of
        Ok tree ->
            tree

        _ ->
            []


getTime : Time.Posix -> String
getTime dateTime =
    let
        day =
            Time.toDay Time.utc dateTime

        month =
            Time.toMonth Time.utc dateTime

        year =
            Time.toYear Time.utc dateTime
    in
    monthToString month ++ " " ++ String.fromInt day ++ ", " ++ String.fromInt year


viewSpinner : Html msg
viewSpinner =
    span [ class "spinner" ] [ Icon.viewStyled [] IconReg.lemon ]


justOrEmpty : Maybe String -> String
justOrEmpty maybe =
    case maybe of
        Just res ->
            res

        Nothing ->
            ""


viewIf : Bool -> Html Msg -> Html Msg
viewIf condition element =
    if condition then
        element

    else
        text ""
