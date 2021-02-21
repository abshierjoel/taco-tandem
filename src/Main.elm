module Main exposing (..)

import Accessibility exposing (h2)
import Browser exposing (Document, element)
import Browser.Dom exposing (Error(..))
import Browser.Navigation as Nav
import CategoryPage as CatPage
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Regular as IconReg
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, nonNullElementsOrFail, nonNullOrFail, with)
import Homepage as Home
import Html exposing (Html, a, button, div, h1, img, menuitem, p, span, text)
import Html.Attributes exposing (class, disabled, href, src, style, width)
import Html.Events exposing (onClick)
import Html.Parser exposing (Node)
import Html.Parser.Util
import Json.Decode exposing (Error(..))
import Maybe.Extra as Maybe
import RemoteData exposing (RemoteData)
import SinglePostPage as SinglePostPage
import String.Extra
import Taco.Enum.PostObjectFieldFormatEnum exposing (PostObjectFieldFormatEnum)
import Taco.Object exposing (Category, Post, RootQueryToPostConnectionEdge)
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
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    updateUrl url (initialModel key)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Homepage homepageModel ->
            Home.subscriptions homepageModel
                |> Sub.map GotHomepageMsg

        _ ->
            Sub.none


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just HomeRoute ->
            toHomepageModel { model | page = Homepage Home.initialModel } (Home.init ())

        Just (CategoryRoute categoryName) ->
            toCategoryPageModel { model | page = CategoryPage CatPage.initialModel } (CatPage.init categoryName)

        Just (PostRoute slug) ->
            toSinglePostModel { model | page = PostPage SinglePostPage.initialModel } (SinglePostPage.init slug)

        Just (PageRoute name) ->
            ( { model | page = Homepage Home.initialModel }, Cmd.none )

        Nothing ->
            ( { model | page = NotFoundPage }, Cmd.none )


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map CategoryRoute (s "c" </> Parser.string)
        , Parser.map PageRoute (s "page" </> Parser.string)
        , Parser.map PostRoute (s "post" </> Parser.string)
        ]


initialModel : Nav.Key -> Model
initialModel key =
    { page = Homepage Home.initialModel
    , showDropDown = False
    , key = key
    , pageTitle = homepageTitle
    }


homepageTitle =
    "Taco Tandem - A Blog to Share the Love of Tacos!"



---- MODEL ----


type Page
    = Homepage Home.Model
    | PostPage SinglePostPage.Model
    | PagePage String
    | CategoryPage CatPage.Model
    | NotFoundPage


type Route
    = HomeRoute
    | CategoryRoute String
    | PostRoute String
    | PageRoute String


type alias Model =
    { page : Page
    , showDropDown : Bool
    , key : Nav.Key
    , pageTitle : String
    }



---- UPDATE ----


type Msg
    = ClickedMenuButton
    | ClickedCloseMenuButton
    | GotHomepageMsg Home.Msg
    | GotCategoryPageMsg CatPage.Msg
    | GotSinglePostPageMsg SinglePostPage.Msg
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedMenuButton ->
            ( { model | showDropDown = not model.showDropDown }, Cmd.none )

        ClickedCloseMenuButton ->
            ( { model | showDropDown = False }, Cmd.none )

        GotHomepageMsg homepageMsg ->
            case model.page of
                Homepage homepageModel ->
                    toHomepageModel model (Home.update homepageMsg homepageModel)

                _ ->
                    ( model, Cmd.none )

        GotCategoryPageMsg categoryPageMsg ->
            case model.page of
                CategoryPage categoryPageModel ->
                    toCategoryPageModel model (CatPage.update categoryPageMsg categoryPageModel)

                _ ->
                    ( model, Cmd.none )

        GotSinglePostPageMsg singlePostMsg ->
            case model.page of
                PostPage singlePostModel ->
                    toSinglePostModel model (SinglePostPage.update singlePostMsg singlePostModel)

                _ ->
                    ( model, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            updateUrl url { model | showDropDown = False }


toHomepageModel : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
toHomepageModel model ( latestPosts, cmd ) =
    ( { model | page = Homepage latestPosts, pageTitle = homepageTitle }
    , Cmd.map GotHomepageMsg cmd
    )


toCategoryPageModel : Model -> ( CatPage.Model, Cmd CatPage.Msg ) -> ( Model, Cmd Msg )
toCategoryPageModel model ( categoryPageModel, cmd ) =
    let
        newTitle =
            String.Extra.toTitleCase categoryPageModel.categoryId ++ " - Taco Tandem"
    in
    ( { model | page = CategoryPage categoryPageModel, pageTitle = newTitle }
    , Cmd.map GotCategoryPageMsg cmd
    )


toSinglePostModel : Model -> ( SinglePostPage.Model, Cmd SinglePostPage.Msg ) -> ( Model, Cmd Msg )
toSinglePostModel model ( singleModel, cmd ) =
    let
        newTitle =
            case singleModel.post of
                RemoteData.Success data ->
                    case data of
                        Just post ->
                            case post.title of
                                Just res ->
                                    res ++ " - Taco Tandem"

                                _ ->
                                    model.pageTitle

                        _ ->
                            model.pageTitle

                _ ->
                    model.pageTitle
    in
    ( { model | page = PostPage singleModel, pageTitle = newTitle }
    , Cmd.map GotSinglePostPageMsg cmd
    )



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        currentPage =
            case model.page of
                Homepage homepageModel ->
                    Home.view homepageModel
                        |> Html.map GotHomepageMsg

                PostPage singlePostModel ->
                    SinglePostPage.view singlePostModel
                        |> Html.map GotSinglePostPageMsg

                CategoryPage categoryPageModel ->
                    CatPage.view categoryPageModel
                        |> Html.map GotCategoryPageMsg

                _ ->
                    viewNotFound
    in
    { title = model.pageTitle
    , body =
        [ div [ class "wrapper" ]
            [ Icon.css
            , viewHeader model.showDropDown
            , currentPage
            ]
        ]
    }


viewHeader : Bool -> Html Msg
viewHeader showDropDown =
    let
        menuItems =
            div [ class "nav-dropdown" ]
                [ a [ href "/", class "nav-button" ] [ Icon.viewStyled [] Icon.home, text "Home" ]
                , a [ href "/c/travels/", class "nav-button" ] [ Icon.viewStyled [] Icon.plane, text "Travel" ]
                , a [ href "/c/recipes/", class "nav-button" ] [ Icon.viewStyled [] Icon.utensils, text "Recipes" ]

                --, a [ href "/about", class "nav-button" ] [ Icon.viewStyled [] Icon.book, text "About" ]
                , button [ class "nav-button", onClick ClickedCloseMenuButton ]
                    [ Icon.viewStyled [] Icon.times, text "Close" ]
                ]
    in
    div [ class "header" ]
        [ a [ href "/" ]
            [ div
                [ class "header-logo animate__animated animate__zoomIn " ]
                [ div [ class "header-icon animate__animated animate__infinite animate__pulse animate__slower" ]
                    [ img [ src "/taco.svg" ] [] ]
                , div [ class "header-text" ]
                    [ span [] [ text " Taco" ]
                    , span [] [ text "Tandem" ]
                    ]
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
                [ a [ href "/", class "nav-button" ] [ Icon.viewStyled [] Icon.home, text "Home" ]
                , a [ href "/c/travels/", class "nav-button" ] [ Icon.viewStyled [] Icon.plane, text "Travel" ]
                , a [ href "/c/recipes/", class "nav-button" ] [ Icon.viewStyled [] Icon.utensils, text "Recipes" ]

                --, a [ href "/about", class "nav-button" ] [ Icon.viewStyled [] Icon.book, text "About" ]
                ]
            ]
        ]


viewNotFound : Html msg
viewNotFound =
    div [ class "page animate__animated animate__backInUp" ]
        [ h1 [] [ Icon.viewStyled [] Icon.frownOpen, text " Oh no! The cilantro is missing!" ]
        , p []
            [ text "We're so sorry, but the page you're looking for doesn't exist or has been moved."
            ]
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
