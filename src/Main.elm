module Main exposing (..)

import Accessibility exposing (h2)
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
import Homepage as Home
import Html exposing (Html, a, button, div, h1, img, menuitem, p, span, text)
import Html.Attributes exposing (class, disabled, href, src, style, width)
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

        Just (PageRoute name) ->
            ( { model | page = Homepage Home.initialModel }, Cmd.none )

        Just (PostRoute name) ->
            ( { model | page = PostPage name }, Cmd.none )

        Nothing ->
            ( { model | page = NotFoundPage }, Cmd.none )


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map PageRoute (s "page" </> Parser.string)
        , Parser.map HomeRoute Parser.top
        , Parser.map PostRoute (s "post" </> Parser.string)
        ]


initialModel : Nav.Key -> Model
initialModel key =
    { page = Homepage Home.initialModel
    , showDropDown = False
    , key = key
    }



---- MODEL ----


type Page
    = Homepage Home.Model
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
    , key : Nav.Key
    }



---- UPDATE ----


type Msg
    = ClickedMenuButton
    | ClickedCloseMenuButton
    | GotHomepageMsg Home.Msg
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

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            updateUrl url model


toHomepageModel : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
toHomepageModel model ( latestPosts, cmd ) =
    ( { model | page = Homepage latestPosts }
    , Cmd.map GotHomepageMsg cmd
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

                _ ->
                    viewNotFound
    in
    { title = "Taco Tandem"
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
                [ a [ href "/", class "nav-button" ] [ Icon.viewStyled [] Icon.home, text "Home" ]
                , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.plane, text "Travel" ]
                , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.utensils, text "Recipes" ]
                , button [ class "nav-button" ] [ Icon.viewStyled [] Icon.book, text "About" ]
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
