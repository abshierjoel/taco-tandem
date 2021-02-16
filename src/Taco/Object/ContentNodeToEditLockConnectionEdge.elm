-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Object.ContentNodeToEditLockConnectionEdge exposing (..)

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

{-| The timestamp for when the node was last edited
-}
lockTimestamp : SelectionSet (Maybe String) Taco.Object.ContentNodeToEditLockConnectionEdge
lockTimestamp =
      Object.selectionForField "(Maybe String)" "lockTimestamp" [] (Decode.string |> Decode.nullable)


{-| The nodes of the connection, without the edges
-}
node : SelectionSet decodesTo Taco.Object.User
 -> SelectionSet (Maybe decodesTo) Taco.Object.ContentNodeToEditLockConnectionEdge
node object____ =
      Object.selectionForCompositeField "node" [] (object____) (identity >> Decode.nullable)
