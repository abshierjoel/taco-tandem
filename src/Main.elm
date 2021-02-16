module Main exposing (..)

import Browser exposing (element)
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html exposing (Html, button, div, h1, img, menuitem, p, span, text)
import Html.Attributes exposing (class, src, style, width)
import Html.Events exposing (onClick)



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
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { showDropDown = False
    }



---- MODEL ----


type alias Model =
    { showDropDown : Bool
    }



---- UPDATE ----


type Msg
    = ClickedMenuButton
    | ClickedCloseMenuButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedMenuButton ->
            ( { model | showDropDown = not model.showDropDown }, Cmd.none )

        ClickedCloseMenuButton ->
            ( { model | showDropDown = False }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
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
        , div [ class "page animate__animated animate__backInUp" ]
            [ h1 [] [ text "Tacos are Tasty" ]
            , span [ class "post-author" ] [ text "By Elizabeth Hale on 02/21/2021" ]
            , div [ class "post-body" ]
                [ p [] [ text "Greetings, and thanks so much for visiting this website! This is the first time that you have seen my blog - we've had many visitors over the years and these days every day it's always some exciting new recipe or restaurant which brings me out in front towards all those food lovers from everywhere who are keen on starting their own cooking tradition within our community! Since 2005 when I started making 'chili' burritos using nothing more than avocado, guacamole etc., then there has been quite an amazing expansion/expansion into other cuisines besides chili (pico de gallo + black beans & rice)." ]
                , p [] [ text "I like tacos carne asada. I like to eat tacos with a lot of lime juice. One morning, when we were looking at different items I like tacos carne asada. I like to eat tacos with a lot of lime juice. One morning, when we were looking at different items on our menu, one had chicken and it reminded me so much that's what my friends do sometimes because they love the flavor but also can't figure out why this dish is called taco al pastor since there are no words for its name or how come all meat dishes in Mexico has something pronounced taowtejo (my daughter calls them Taco Peppers). We have an entire recipe book dedicated full stop just about making everything from Mexican food taste exactly like Tex-Mex style! My kids will go crazy over those recipes – not only may their mom buy more stuff she loves - including cilantr on our menu, one had chicken and it reminded me so much that's what my friends do sometimes because they love the flavor but also can't figure out why this dish is called taco al pastor since there are no words for its name or how come all meat dishes in Mexico has something pronounced taowtejo (my daughter calls them Taco Peppers)." ]
                , p [] [ text "We have an entire recipe book dedicated full stop just about making everything from Mexican food taste exactly like Tex-Mex style! My kids will go crazy over those recipes – not only may their mom buy more stuff she loves - including cilantro." ]
                ]
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
        ]


viewIf : Bool -> Html Msg -> Html Msg
viewIf condition element =
    if condition then
        element

    else
        text ""
