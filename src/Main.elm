module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, p, span, text)
import Html.Attributes exposing (class, src, style, width)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ div [ class "header" ]
            [ div [ class "header-logo animate__animated animate__zoomIn " ]
                [ div [ class "header-icon animate__animated animate__infinite animate__pulse animate__slower" ]
                    [ img [ src "./taco.svg" ] [] ]
                , div [ class "header-text" ]
                    [ span [] [ text " Taco" ]
                    , span [] [ text "Tandem." ]
                    ]
                ]
            , div [ class "nav-wrapper" ]
                [ div [ class "header-nav header-icon animate__animated animate__backInLeft" ]
                    [ button [ class "nav-button" ] [ text "Home" ]
                    , button [ class "nav-button" ] [ text "Travel" ]
                    , button [ class "nav-button" ] [ text "Recipes" ]
                    , button [ class "nav-button" ] [ text "About" ]
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
            , div [ class "post-about" ] [ text "Elizabeth Hale" ]
            ]
        ]
