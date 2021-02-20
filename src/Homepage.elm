module Homepage exposing (..)

import Accessibility as Html exposing (Html, button, div, h1, span, text)
import Browser
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Regular as IconReg
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, nonNullElementsOrFail, nonNullOrFail, with)
import Html exposing (Html)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Html.Parser exposing (Node)
import Html.Parser.Util
import RemoteData exposing (RemoteData)
import Taco.Enum.PostObjectFieldFormatEnum exposing (PostObjectFieldFormatEnum)
import Taco.Object exposing (Post, RootQueryToPostConnectionEdge)
import Taco.Object.Post as Post
import Taco.Object.RootQueryToPostConnection
import Taco.Object.RootQueryToPostConnectionEdge as Edge
import Taco.Object.User as User
import Taco.Object.WPPageInfo
import Taco.Query as Query exposing (posts)
import Taco.Scalar exposing (Id)



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
    }



---- UPDATE ----


type Msg
    = DoNothing
    | GotResponse (RemoteData (Graphql.Http.Error TestResponse) TestResponse)
    | GotMorePostsResponse (RemoteData (Graphql.Http.Error TestResponse) TestResponse)
    | ClickedLoadMore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

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
        |> Graphql.Http.queryRequest "http://localhost/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


getMorePosts : String -> Cmd Msg
getMorePosts cursor =
    postsQuery cursor
        |> Graphql.Http.queryRequest "http://localhost/graphql"
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
    SelectionSet.map5 Post
        Post.date
        Post.commentCount
        Post.uri
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
                    text "Failed to load blog posts"

                RemoteData.Loading ->
                    text "Loading Posts"

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


getParsedHtml : String -> List Node
getParsedHtml html =
    case Html.Parser.run html of
        Ok tree ->
            tree

        _ ->
            []


viewPost : Post -> Html Msg
viewPost post =
    let
        title =
            justOrEmpty post.title

        date =
            justOrEmpty post.date

        content =
            justOrEmpty post.content
    in
    div [ class "post" ]
        [ h1 [ class "post-title" ] [ text title ]
        , span [ class "post-author" ] [ text <| "By Elizabeth Hale on " ++ date ]
        , div [ class "post-body" ]
            (Html.Parser.Util.toVirtualDom (getParsedHtml content))

        -- , div [ class "post-about" ]
        --     [ div [ class "about-image" ]
        --         [ img [ src "./elizabeth.jpg" ] [] ]
        --     , div [ class "about-text" ]
        --         [ span [ class "text-bold text-large stack-m" ] [ text "Elizabeth Hale" ]
        --         , span [ class "text-italic text-light stack-s" ] [ text "Taco Consumer, Part-time Genius, Taco Developer, Author" ]
        --         , p [ class "" ] [ text "She seems to have the same skills as you.\" Now I know that by studying, they mean math where no one can actually do it well or accurately and if there are any mistakes made—no matter how simple of an error could cause someone harm; when some student has done horrible things in class just because he wants to get into college…it would be unthinkable for them not only academically but emotionally too,\"" ]
        --         , p [ class "" ] [ text "The teacher said with certainty and assurance. It's true: every person studied did terrible things during his early childhood including bad behavior on homework (not counting all those nights hanging out at recess), beating up your parents over trivial questions" ]
        --         ]
        --     ]
        ]


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
