-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Taco.Enum.MediaItemSizeEnum exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The size of the media item object.

  - Large - MediaItem with the large size
  - Medium - MediaItem with the medium size
  - MediumLarge - MediaItem with the medium\_large size
  - PostThumbnail - MediaItem with the post-thumbnail size
  - Thumbnail - MediaItem with the thumbnail size
  - 1536x1536\_ - MediaItem with the 1536x1536 size
  - 2048x2048\_ - MediaItem with the 2048x2048 size

-}
type MediaItemSizeEnum
    = Large
    | Medium
    | MediumLarge
    | PostThumbnail
    | Thumbnail


list : List MediaItemSizeEnum
list =
    [ Large, Medium, MediumLarge, PostThumbnail, Thumbnail ]


decoder : Decoder MediaItemSizeEnum
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "LARGE" ->
                        Decode.succeed Large

                    "MEDIUM" ->
                        Decode.succeed Medium

                    "MEDIUM_LARGE" ->
                        Decode.succeed MediumLarge

                    "POST_THUMBNAIL" ->
                        Decode.succeed PostThumbnail

                    "THUMBNAIL" ->
                        Decode.succeed Thumbnail

                    _ ->
                        Decode.fail ("Invalid MediaItemSizeEnum type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : MediaItemSizeEnum -> String
toString enum____ =
    case enum____ of
        Large ->
            "LARGE"

        Medium ->
            "MEDIUM"

        MediumLarge ->
            "MEDIUM_LARGE"

        PostThumbnail ->
            "POST_THUMBNAIL"

        Thumbnail ->
            "THUMBNAIL"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe MediaItemSizeEnum
fromString enumString____ =
    case enumString____ of
        "LARGE" ->
            Just Large

        "MEDIUM" ->
            Just Medium

        "MEDIUM_LARGE" ->
            Just MediumLarge

        "POST_THUMBNAIL" ->
            Just PostThumbnail

        "THUMBNAIL" ->
            Just Thumbnail

        _ ->
            Nothing
