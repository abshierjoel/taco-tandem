-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Enum.PostObjectsConnectionDateColumnEnum exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The column to use when filtering by date
-}
type PostObjectsConnectionDateColumnEnum
    = Date
    | Modified
list : List PostObjectsConnectionDateColumnEnum
list =
    [Date, Modified]
decoder : Decoder PostObjectsConnectionDateColumnEnum
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "DATE" ->
                        Decode.succeed Date

                    "MODIFIED" ->
                        Decode.succeed Modified

                    _ ->
                        Decode.fail ("Invalid PostObjectsConnectionDateColumnEnum type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
        )
        

{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : PostObjectsConnectionDateColumnEnum -> String
toString enum____ =
    case enum____ of
        Date ->
                "DATE"


        Modified ->
                "MODIFIED"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe PostObjectsConnectionDateColumnEnum
fromString enumString____ =
    case enumString____ of
        "DATE" ->
                Just Date


        "MODIFIED" ->
                Just Modified

        _ ->
                Nothing
