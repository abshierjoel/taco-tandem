-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Enum.UserNodeIdTypeEnum exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The Type of Identifier used to fetch a single User node. To be used along with the "id" field. Default is "ID".

  - DatabaseId - The Database ID for the node
  - Email - The Email of the User
  - Id - The hashed Global ID
  - Slug - The slug of the User
  - Uri - The URI for the node
  - Username - The username the User uses to login with

-}
type UserNodeIdTypeEnum
    = DatabaseId
    | Email
    | Id
    | Slug
    | Uri
    | Username
list : List UserNodeIdTypeEnum
list =
    [DatabaseId, Email, Id, Slug, Uri, Username]
decoder : Decoder UserNodeIdTypeEnum
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "DATABASE_ID" ->
                        Decode.succeed DatabaseId

                    "EMAIL" ->
                        Decode.succeed Email

                    "ID" ->
                        Decode.succeed Id

                    "SLUG" ->
                        Decode.succeed Slug

                    "URI" ->
                        Decode.succeed Uri

                    "USERNAME" ->
                        Decode.succeed Username

                    _ ->
                        Decode.fail ("Invalid UserNodeIdTypeEnum type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
        )
        

{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : UserNodeIdTypeEnum -> String
toString enum____ =
    case enum____ of
        DatabaseId ->
                "DATABASE_ID"


        Email ->
                "EMAIL"


        Id ->
                "ID"


        Slug ->
                "SLUG"


        Uri ->
                "URI"


        Username ->
                "USERNAME"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe UserNodeIdTypeEnum
fromString enumString____ =
    case enumString____ of
        "DATABASE_ID" ->
                Just DatabaseId


        "EMAIL" ->
                Just Email


        "ID" ->
                Just Id


        "SLUG" ->
                Just Slug


        "URI" ->
                Just Uri


        "USERNAME" ->
                Just Username

        _ ->
                Nothing
