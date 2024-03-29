-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Enum.TaxonomyIdTypeEnum exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The Type of Identifier used to fetch a single Taxonomy node. To be used along with the "id" field. Default is "ID".

  - Id - The globally unique ID
  - Name - The name of the taxonomy

-}
type TaxonomyIdTypeEnum
    = Id
    | Name
list : List TaxonomyIdTypeEnum
list =
    [Id, Name]
decoder : Decoder TaxonomyIdTypeEnum
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ID" ->
                        Decode.succeed Id

                    "NAME" ->
                        Decode.succeed Name

                    _ ->
                        Decode.fail ("Invalid TaxonomyIdTypeEnum type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
        )
        

{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : TaxonomyIdTypeEnum -> String
toString enum____ =
    case enum____ of
        Id ->
                "ID"


        Name ->
                "NAME"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe TaxonomyIdTypeEnum
fromString enumString____ =
    case enumString____ of
        "ID" ->
                Just Id


        "NAME" ->
                Just Name

        _ ->
                Nothing
