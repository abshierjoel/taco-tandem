module Homepage exposing (..)

import Accessibility as Html exposing (Html, button, div, h1, span, text)
import Browser
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Regular as IconReg
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, nonNullElementsOrFail, nonNullOrFail, with)
import Html exposing (Html, a)
import Html.Attributes exposing (class, disabled, href)
import Html.Attributes.Aria exposing (ariaHidden)
import Html.Events exposing (onClick)
import Html.Parser exposing (Node)
import Html.Parser.Util
import Iso8601
import RemoteData exposing (RemoteData)
import SocialLinks exposing (viewShareButtons)
import Taco.Enum.PostObjectFieldFormatEnum
import Taco.Object exposing (Post)
import Taco.Object.NodeWithAuthorToUserConnectionEdge as NodeWithAuthorToUserConnectionEdge
import Taco.Object.Post as Post
import Taco.Object.RootQueryToPostConnection
import Taco.Object.RootQueryToPostConnectionEdge as Edge
import Taco.Object.User as User
import Taco.Object.WPPageInfo
import Taco.Query as Query exposing (posts)
import Time exposing (Month(..))



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



---- INIT ----


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel, getPosts "" )


initialModel =
    { postsResponse = RemoteData.Loading
    , morePostsResponse = RemoteData.NotAsked
    , posts = []
    , lastCursor = ""
    , hasNextPage = False
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


graphqlEndpoint =
    "/wordpress/graphql"



---- MODEL ----


type alias Model =
    { postsResponse : RemoteData (Graphql.Http.Error TestResponse) TestResponse
    , morePostsResponse : RemoteData (Graphql.Http.Error TestResponse) TestResponse
    , posts : List Post
    , lastCursor : String
    , hasNextPage : Bool
    }


type alias TestResponse =
    Maybe Response


type alias Response =
    Paginator (List Post) String


type alias Paginator dataType cursorType =
    { data : dataType
    , paginationData : PaginationData cursorType
    }


type alias PaginationData cursorType =
    { cursor : Maybe cursorType
    , hasNextPage : Bool
    }


type alias Posts =
    Maybe (List Post)


type alias Post =
    { date : Maybe String
    , commentCount : Maybe Int
    , uri : String
    , title : Maybe String
    , content : Maybe String
    , preview : Preview
    , author : Maybe User
    }


type alias Preview =
    { title : Maybe String
    , content : Maybe String
    }


type alias User =
    { firstName : Maybe String
    , nickname : Maybe String
    , description : Maybe String
    }



---- UPDATE ----


type Msg
    = GotResponse (RemoteData (Graphql.Http.Error TestResponse) TestResponse)
    | GotMorePostsResponse (RemoteData (Graphql.Http.Error TestResponse) TestResponse)
    | ClickedLoadMore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            case response of
                RemoteData.Success data ->
                    let
                        posts =
                            case data of
                                Just res ->
                                    res.data

                                Nothing ->
                                    []

                        hasNextPage =
                            case data of
                                Just res ->
                                    res.paginationData.hasNextPage

                                Nothing ->
                                    True

                        lastCursor =
                            case data of
                                Just res ->
                                    justOrEmpty res.paginationData.cursor

                                Nothing ->
                                    ""
                    in
                    ( { model | postsResponse = response, posts = posts, hasNextPage = hasNextPage, lastCursor = lastCursor }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotMorePostsResponse response ->
            case response of
                RemoteData.Success data ->
                    let
                        posts =
                            case data of
                                Just res ->
                                    List.concat [ model.posts, res.data ]

                                Nothing ->
                                    []

                        hasNextPage =
                            case data of
                                Just res ->
                                    res.paginationData.hasNextPage

                                Nothing ->
                                    True

                        lastCursor =
                            case data of
                                Just res ->
                                    justOrEmpty res.paginationData.cursor

                                Nothing ->
                                    ""
                    in
                    ( { model | morePostsResponse = response, posts = posts, hasNextPage = hasNextPage, lastCursor = lastCursor }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClickedLoadMore ->
            ( { model | morePostsResponse = RemoteData.Loading }, getMorePosts model.lastCursor )



---- QUERY ----


getPosts : String -> Cmd Msg
getPosts cursor =
    postsQuery cursor
        |> Graphql.Http.queryRequest graphqlEndpoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


getMorePosts : String -> Cmd Msg
getMorePosts cursor =
    postsQuery cursor
        |> Graphql.Http.queryRequest graphqlEndpoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotMorePostsResponse)


postsQuery : String -> SelectionSet (Maybe Response) RootQuery
postsQuery cursor =
    Query.posts (getPostsOptArgs cursor) postsSelection


getPostsOptArgs : String -> Query.PostsOptionalArguments -> Query.PostsOptionalArguments
getPostsOptArgs cursor args =
    { first = Graphql.OptionalArgument.Present 5
    , last = Null
    , after = Graphql.OptionalArgument.Present cursor
    , before = Null
    , where_ = Null
    }


postsSelection : SelectionSet Response Taco.Object.RootQueryToPostConnection
postsSelection =
    SelectionSet.succeed Paginator
        |> with postsSelectionEdges
        |> with (Taco.Object.RootQueryToPostConnection.pageInfo pageInfoSelection |> nonNullOrFail)


pageInfoSelection : SelectionSet (PaginationData String) Taco.Object.WPPageInfo
pageInfoSelection =
    SelectionSet.succeed PaginationData
        |> with Taco.Object.WPPageInfo.endCursor
        |> with Taco.Object.WPPageInfo.hasNextPage


postsSelectionEdges : SelectionSet (List Post) Taco.Object.RootQueryToPostConnection
postsSelectionEdges =
    Taco.Object.RootQueryToPostConnection.edges
        (Edge.node postSelection
            |> SelectionSet.nonNullOrFail
        )
        |> nonNullOrFail
        |> nonNullElementsOrFail


postSelection : SelectionSet Post Taco.Object.Post
postSelection =
    SelectionSet.map7 Post
        Post.date
        Post.commentCount
        Post.uri
        (Post.title postTitleArgs)
        (Post.content postContentArgs)
        previewSelection
        (Post.author authorSelectionEdge)


authorSelectionEdge : SelectionSet User Taco.Object.NodeWithAuthorToUserConnectionEdge
authorSelectionEdge =
    NodeWithAuthorToUserConnectionEdge.node authorSelection
        |> nonNullOrFail


authorSelection : SelectionSet User Taco.Object.User
authorSelection =
    SelectionSet.map3 User
        User.firstName
        User.nickname
        User.description


previewSelection : SelectionSet Preview Taco.Object.Post
previewSelection =
    SelectionSet.map2 Preview
        (Post.title postTitleArgs)
        (Post.content postContentArgs)


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
        postList =
            case model.postsResponse of
                RemoteData.Success result ->
                    let
                        postsInfo =
                            case result of
                                Just res ->
                                    ( model.posts, model.hasNextPage )

                                Nothing ->
                                    ( [], False )
                    in
                    viewPostList model postsInfo

                RemoteData.Failure _ ->
                    div [] [ h1 [] [ text "Sorry! The tacos are lost!" ], text "Failed to load blog posts" ]

                RemoteData.Loading ->
                    div [ class "page-spinner" ]
                        [ viewSpinner
                        , span [ class "sr-only" ] [ text "Loading posts..." ]
                        , span [ ariaHidden True ] [ text " Prepping Tacos" ]
                        ]

                RemoteData.NotAsked ->
                    text "Loading Posts"
    in
    div [ class "page animate__animated animate__backInUp" ]
        [ postList ]


viewPostList : Model -> ( List Post, Bool ) -> Html Msg
viewPostList model ( posts, hasMore ) =
    let
        isLoading =
            case model.morePostsResponse of
                RemoteData.Loading ->
                    True

                _ ->
                    False

        buttonText =
            if isLoading then
                "Prepping Tacos..."

            else
                "Load More Posts"
    in
    div []
        [ div [] (List.map viewPost posts)
        , viewIf hasMore
            (button [ class "load-more", onClick ClickedLoadMore, disabled isLoading ]
                [ viewIf isLoading viewSpinner, text buttonText ]
            )
        ]


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

        content =
            justOrEmpty post.content
    in
    div [ class "post" ]
        [ a [ href <| "/post" ++ post.uri ]
            [ h1 [ class "post-title" ] [ text title ]
            ]
        , div [ class "post-subtitle" ]
            [ div [ class "post-info" ] [ text postInfo ]
            , viewShareButtons post.uri title
            ]
        , div [ class "post-body" ]
            (Html.Parser.Util.toVirtualDom (getParsedHtml content))
        , div [ class "post-separator" ] []
        ]


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
