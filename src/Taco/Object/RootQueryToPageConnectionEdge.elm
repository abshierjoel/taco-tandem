-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Object.RootQueryToPageConnectionEdge exposing (..)

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

{-| A cursor for use in pagination
-}
cursor : SelectionSet (Maybe String) Taco.Object.RootQueryToPageConnectionEdge
cursor =
      Object.selectionForField "(Maybe String)" "cursor" [] (Decode.string |> Decode.nullable)


{-| The item at the end of the edge
-}
node : SelectionSet decodesTo Taco.Object.Page
 -> SelectionSet (Maybe decodesTo) Taco.Object.RootQueryToPageConnectionEdge
node object____ =
      Object.selectionForCompositeField "node" [] (object____) (identity >> Decode.nullable)
