-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Enum.MediaItemStatusEnum exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The status of the media item object.

  - AutoDraft - Objects with the auto-draft status
  - Inherit - Objects with the inherit status
  - Private - Objects with the private status
  - Trash - Objects with the trash status

-}
type MediaItemStatusEnum
    = AutoDraft
    | Inherit
    | Private
    | Trash
list : List MediaItemStatusEnum
list =
    [AutoDraft, Inherit, Private, Trash]
decoder : Decoder MediaItemStatusEnum
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "AUTO_DRAFT" ->
                        Decode.succeed AutoDraft

                    "INHERIT" ->
                        Decode.succeed Inherit

                    "PRIVATE" ->
                        Decode.succeed Private

                    "TRASH" ->
                        Decode.succeed Trash

                    _ ->
                        Decode.fail ("Invalid MediaItemStatusEnum type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
        )
        

{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : MediaItemStatusEnum -> String
toString enum____ =
    case enum____ of
        AutoDraft ->
                "AUTO_DRAFT"


        Inherit ->
                "INHERIT"


        Private ->
                "PRIVATE"


        Trash ->
                "TRASH"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe MediaItemStatusEnum
fromString enumString____ =
    case enumString____ of
        "AUTO_DRAFT" ->
                Just AutoDraft


        "INHERIT" ->
                Just Inherit


        "PRIVATE" ->
                Just Private


        "TRASH" ->
                Just Trash

        _ ->
                Nothing
