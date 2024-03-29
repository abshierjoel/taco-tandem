-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Object.WritingSettings exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.SelectionSet exposing (SelectionSet)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Taco.Object
import Taco.Interface
import Taco.Union
import Taco.Scalar
import Taco.InputObject
import Taco.ScalarCodecs
import Json.Decode as Decode
import Graphql.Internal.Encode as Encode exposing (Value)

{-| Default post category.
-}
defaultCategory : SelectionSet (Maybe Int) Taco.Object.WritingSettings
defaultCategory =
      Object.selectionForField "(Maybe Int)" "defaultCategory" [] (Decode.int |> Decode.nullable)


{-| Default post format.
-}
defaultPostFormat : SelectionSet (Maybe String) Taco.Object.WritingSettings
defaultPostFormat =
      Object.selectionForField "(Maybe String)" "defaultPostFormat" [] (Decode.string |> Decode.nullable)


{-| Convert emoticons like :-) and :-P to graphics on display.
-}
useSmilies : SelectionSet (Maybe Bool) Taco.Object.WritingSettings
useSmilies =
      Object.selectionForField "(Maybe Bool)" "useSmilies" [] (Decode.bool |> Decode.nullable)
