module Main exposing (..)

import Browser exposing (element)
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, nonNullElementsOrFail, nonNullOrFail, with)
import Html exposing (Html, button, div, h1, img, menuitem, p, span, text)
import Html.Attributes exposing (class, src, style, width)
import Html.Events exposing (onClick)
import Html.Parser exposing (Node)
import Html.Parser.Util
import Json.Decode exposing (Error(..))
import RemoteData exposing (RemoteData)
import Taco.Enum.PostObjectFieldFormatEnum exposing (PostObjectFieldFormatEnum)
import Taco.Object
import Taco.Object.Post as Post
import Taco.Object.RootQueryToPostConnection
import Taco.Object.User as User
import Taco.Query as Query
import Taco.Scalar exposing (Id)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getPosts )


initialModel : Model
initialModel =
    { showDropDown = False
    , response = RemoteData.Loading
    }



---- MODEL ----


type alias Model =
    { showDropDown : Bool
    , response : RemoteData (Graphql.Http.Error Response) Response
    }



---- UPDATE ----


type Msg
    = ClickedMenuButton
    | ClickedCloseMenuButton
    | GotResponse (RemoteData (Graphql.Http.Error Response) Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedMenuButton ->
            ( { model | showDropDown = not model.showDropDown }, Cmd.none )

        ClickedCloseMenuButton ->
            ( { model | showDropDown = False }, Cmd.none )

        GotResponse response ->
            ( { model | response = response }, Cmd.none )



---- QUERY ----


type alias Response =
    Maybe (List Post)


getPosts : Cmd Msg
getPosts =
    postsQuery
        |> Graphql.Http.queryRequest "http://localhost/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)



-- getUser : Cmd Msg
-- getUser =
--     query
--         |> Graphql.Http.queryRequest "http://localhost/graphql"
--         |> Graphql.Http.send (RemoteData.fromResult >> GotUser)
-- type alias User =
--     { firstName : Maybe String
--     , lastName : Maybe String
--     , username : Maybe String
--     }
-- type alias Users =
--     { edges : List User
--     }
-- query : SelectionSet Response RootQuery
-- query =
--     Query.user getOptArgs getReqArgs userSelection
-- getOptArgs : Query.UserOptionalArguments -> Query.UserOptionalArguments
-- getOptArgs args =
--     { idType = Null }
-- getReqArgs : Query.UserRequiredArguments
-- getReqArgs =
--     { id = getId }
-- getId : Id
-- getId =
--     Taco.Scalar.Id "dXNlcjox"
-- userSelection : SelectionSet User Taco.Object.User
-- userSelection =
--     SelectionSet.map3 User
--         User.firstName
--         User.lastName
--         User.username


type alias Posts =
    Maybe (List Post)


type alias Post =
    { date : Maybe String
    , commentCount : Maybe Int
    , uri : String
    , title : Maybe String
    , content : Maybe String
    }


postsQuery : SelectionSet (Maybe (List Post)) RootQuery
postsQuery =
    Query.posts getPostsOptArgs postsSelection


getPostsOptArgs : Query.PostsOptionalArguments -> Query.PostsOptionalArguments
getPostsOptArgs args =
    { first = Null
    , last = Null
    , after = Null
    , before = Null
    , where_ = Null
    }


postsSelection : SelectionSet (List Post) Taco.Object.RootQueryToPostConnection
postsSelection =
    Taco.Object.RootQueryToPostConnection.nodes postSelection
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
            case model.response of
                RemoteData.Success result ->
                    let
                        list =
                            case result of
                                Just res ->
                                    res

                                Nothing ->
                                    []
                    in
                    List.map viewPost list

                RemoteData.Failure _ ->
                    [ text "Failed to load blog posts" ]

                RemoteData.Loading ->
                    [ text "Loading Posts" ]

                RemoteData.NotAsked ->
                    [ text "Loading Posts" ]

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
    div [ class "wrapper" ]
        [ Icon.css
        , div [ class "header" ]
            [ div [ class "header-logo animate__animated animate__zoomIn " ]
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
                , viewIf model.showDropDown menuItems
                , div [ class "header-nav header-icon animate__animated animate__backInLeft" ]
                    [ button [ class "nav-button" ] [ Icon.viewStyled [] Icon.home, text "Home" ]
                    , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.plane, text "Travel" ]
                    , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.utensils, text "Recipes" ]
                    , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.book, text "About" ]
                    ]
                ]
            ]
        , div [ class "page animate__animated animate__backInUp" ] postList
        ]


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
        [ h1 [] [ text title ]
        , span [ class "post-author" ] [ text <| "By Elizabeth Hale on " ++ date ]
        , div [ class "post-body" ]
            (Html.Parser.Util.toVirtualDom (getParsedHtml content))
        , div [ class "post-about" ]
            [ div [ class "about-image" ]
                [ img [ src "./elizabeth.jpg" ] [] ]
            , div [ class "about-text" ]
                [ span [ class "text-bold text-large stack-m" ] [ text "Elizabeth Hale" ]
                , span [ class "text-italic text-light stack-s" ] [ text "Taco Consumer, Part-time Genius, Taco Developer, Author" ]
                , p [ class "" ] [ text "She seems to have the same skills as you.\" Now I know that by studying, they mean math where no one can actually do it well or accurately and if there are any mistakes made—no matter how simple of an error could cause someone harm; when some student has done horrible things in class just because he wants to get into college…it would be unthinkable for them not only academically but emotionally too,\"" ]
                , p [ class "" ] [ text "The teacher said with certainty and assurance. It's true: every person studied did terrible things during his early childhood including bad behavior on homework (not counting all those nights hanging out at recess), beating up your parents over trivial questions" ]
                ]
            ]
        ]


viewIf : Bool -> Html Msg -> Html Msg
viewIf condition element =
    if condition then
        element

    else
        text ""
