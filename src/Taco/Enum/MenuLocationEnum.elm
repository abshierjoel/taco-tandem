-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Enum.MenuLocationEnum exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Registered menu locations
-}
type MenuLocationEnum
    = Footer
    | Primary
list : List MenuLocationEnum
list =
    [Footer, Primary]
decoder : Decoder MenuLocationEnum
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "FOOTER" ->
                        Decode.succeed Footer

                    "PRIMARY" ->
                        Decode.succeed Primary

                    _ ->
                        Decode.fail ("Invalid MenuLocationEnum type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
        )
        

{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : MenuLocationEnum -> String
toString enum____ =
    case enum____ of
        Footer ->
                "FOOTER"


        Primary ->
                "PRIMARY"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe MenuLocationEnum
fromString enumString____ =
    case enumString____ of
        "FOOTER" ->
                Just Footer


        "PRIMARY" ->
                Just Primary

        _ ->
                Nothing
