module Main exposing (..)

import Browser exposing (Document, element)
import Browser.Dom exposing (Error(..))
import Browser.Navigation as Nav
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Regular as IconReg
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, nonNullElementsOrFail, nonNullOrFail, with)
import Html exposing (Html, a, button, div, h1, img, menuitem, p, span, text)
import Html.Attributes exposing (class, disabled, src, style, width)
import Html.Events exposing (onClick)
import Html.Parser exposing (Node)
import Html.Parser.Util
import Json.Decode exposing (Error(..))
import Maybe.Extra as Maybe
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
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = \_ -> Debug.todo "Handle URL Requests"
        , onUrlChange = \_ -> Debug.todo "Handle URL Changes"
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    updateUrl url initialModel


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just HomeRoute ->
            ( { initialModel | page = Homepage }, getPosts "" )

        Just (PageRoute name) ->
            ( { initialModel | page = Homepage }, Cmd.none )

        Just (PostRoute name) ->
            ( { initialModel | page = PostPage name }, Cmd.none )

        Nothing ->
            ( { initialModel | page = NotFoundPage }, Cmd.none )


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map PageRoute (s "page" </> Parser.string)
        , Parser.map HomeRoute Parser.top
        , Parser.map PostRoute (s "post" </> Parser.string)
        ]


initialModel : Model
initialModel =
    { page = Homepage
    , showDropDown = False
    , postsResponse = RemoteData.Loading
    , morePostsResponse = RemoteData.NotAsked
    , posts = []
    , lastCursor = ""
    , hasNextPage = False
    }



---- MODEL ----


type Page
    = Homepage
    | PostPage String
    | PagePage String
    | NotFoundPage


type Route
    = HomeRoute
    | PostRoute String
    | PageRoute String


type alias Model =
    { page : Page
    , showDropDown : Bool
    , postsResponse : RemoteData (Graphql.Http.Error TestResponse) TestResponse
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
    = ClickedMenuButton
    | ClickedCloseMenuButton
    | GotResponse (RemoteData (Graphql.Http.Error TestResponse) TestResponse)
    | GotMorePostsResponse (RemoteData (Graphql.Http.Error TestResponse) TestResponse)
    | ClickedLoadMore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedMenuButton ->
            ( { model | showDropDown = not model.showDropDown }, Cmd.none )

        ClickedCloseMenuButton ->
            ( { model | showDropDown = False }, Cmd.none )

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


view : Model -> Document Msg
view model =
    { title = "Taco Tandem"
    , body = [ viewMain model ]
    }


viewMain : Model -> Html Msg
viewMain model =
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
    div [ class "wrapper" ]
        [ Icon.css
        , viewHeader model.showDropDown
        , div [ class "page animate__animated animate__backInUp" ]
            [ postList ]
        ]


viewHeader : Bool -> Html Msg
viewHeader showDropDown =
    let
        menuItems =
            div [ class "nav-dropdown" ]
                [ button [ class "nav-button" ] [ Icon.viewStyled [] Icon.home, text "Home" ]
                , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.plane, text "Travel" ]
                , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.utensils, text "Recipes" ]
                , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.book, text "About" ]
                , button [ class "nav-button", onClick ClickedCloseMenuButton ]
                    [ Icon.viewStyled [] Icon.times, text "Close" ]
                ]
    in
    div [ class "header" ]
        [ div
            [ class "header-logo animate__animated animate__zoomIn " ]
            [ div [ class "header-icon animate__animated animate__infinite animate__pulse animate__slower" ]
                [ img [ src "./taco.svg" ] [] ]
            , div [ class "header-text" ]
                [ span [] [ text " Taco" ]
                , span [] [ text "Tandem" ]
                ]
            ]
        , div [ class "nav-wrapper" ]
            [ div [ class "mobile-header-nav header-icon animate__animated animate__backInLeft" ]
                [ button [ class "menu-button", onClick ClickedMenuButton ]
                    [ Icon.viewStyled [] Icon.bars
                    , text " Menu"
                    ]
                ]
            , viewIf showDropDown menuItems
            , div [ class "header-nav header-icon animate__animated animate__backInLeft" ]
                [ button [ class "nav-button" ] [ Icon.viewStyled [] Icon.home, text "Home" ]
                , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.plane, text "Travel" ]
                , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.utensils, text "Recipes" ]
                , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.book, text "About" ]
                ]
            ]
        ]


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


viewIf : Bool -> Html Msg -> Html Msg
viewIf condition element =
    if condition then
        element

    else
        text ""
