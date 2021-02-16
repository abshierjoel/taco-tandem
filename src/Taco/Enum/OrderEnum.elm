-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Enum.OrderEnum exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The cardinality of the connection order
-}
type OrderEnum
    = Asc
    | Desc
list : List OrderEnum
list =
    [Asc, Desc]
decoder : Decoder OrderEnum
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ASC" ->
                        Decode.succeed Asc

                    "DESC" ->
                        Decode.succeed Desc

                    _ ->
                        Decode.fail ("Invalid OrderEnum type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
        )
        

{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : OrderEnum -> String
toString enum____ =
    case enum____ of
        Asc ->
                "ASC"


        Desc ->
                "DESC"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe OrderEnum
fromString enumString____ =
    case enumString____ of
        "ASC" ->
                Just Asc


        "DESC" ->
                Just Desc

        _ ->
                Nothing
