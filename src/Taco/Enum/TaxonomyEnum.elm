-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Enum.TaxonomyEnum exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Allowed taxonomies
-}
type TaxonomyEnum
    = Category
    | Postformat
    | Tag
list : List TaxonomyEnum
list =
    [Category, Postformat, Tag]
decoder : Decoder TaxonomyEnum
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "CATEGORY" ->
                        Decode.succeed Category

                    "POSTFORMAT" ->
                        Decode.succeed Postformat

                    "TAG" ->
                        Decode.succeed Tag

                    _ ->
                        Decode.fail ("Invalid TaxonomyEnum type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
        )
        

{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : TaxonomyEnum -> String
toString enum____ =
    case enum____ of
        Category ->
                "CATEGORY"


        Postformat ->
                "POSTFORMAT"


        Tag ->
                "TAG"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe TaxonomyEnum
fromString enumString____ =
    case enumString____ of
        "CATEGORY" ->
                Just Category


        "POSTFORMAT" ->
                Just Postformat


        "TAG" ->
                Just Tag

        _ ->
                Nothing
