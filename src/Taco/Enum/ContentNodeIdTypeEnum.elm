-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Enum.ContentNodeIdTypeEnum exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The Type of Identifier used to fetch a single resource. Default is ID.

  - DatabaseId - Identify a resource by the Database ID.
  - Id - Identify a resource by the (hashed) Global ID.
  - Uri - Identify a resource by the URI.

-}
type ContentNodeIdTypeEnum
    = DatabaseId
    | Id
    | Uri
list : List ContentNodeIdTypeEnum
list =
    [DatabaseId, Id, Uri]
decoder : Decoder ContentNodeIdTypeEnum
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "DATABASE_ID" ->
                        Decode.succeed DatabaseId

                    "ID" ->
                        Decode.succeed Id

                    "URI" ->
                        Decode.succeed Uri

                    _ ->
                        Decode.fail ("Invalid ContentNodeIdTypeEnum type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
        )
        

{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ContentNodeIdTypeEnum -> String
toString enum____ =
    case enum____ of
        DatabaseId ->
                "DATABASE_ID"


        Id ->
                "ID"


        Uri ->
                "URI"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ContentNodeIdTypeEnum
fromString enumString____ =
    case enumString____ of
        "DATABASE_ID" ->
                Just DatabaseId


        "ID" ->
                Just Id


        "URI" ->
                Just Uri

        _ ->
                Nothing
