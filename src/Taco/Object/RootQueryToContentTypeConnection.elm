-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Object.RootQueryToContentTypeConnection exposing (..)

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

{-| Edges for the RootQueryToContentTypeConnection connection
-}
edges : SelectionSet decodesTo Taco.Object.RootQueryToContentTypeConnectionEdge
 -> SelectionSet (Maybe (List (Maybe decodesTo))) Taco.Object.RootQueryToContentTypeConnection
edges object____ =
      Object.selectionForCompositeField "edges" [] (object____) (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| The nodes of the connection, without the edges
-}
nodes : SelectionSet decodesTo Taco.Object.ContentType
 -> SelectionSet (Maybe (List (Maybe decodesTo))) Taco.Object.RootQueryToContentTypeConnection
nodes object____ =
      Object.selectionForCompositeField "nodes" [] (object____) (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| Information about pagination in a connection.
-}
pageInfo : SelectionSet decodesTo Taco.Object.WPPageInfo
 -> SelectionSet (Maybe decodesTo) Taco.Object.RootQueryToContentTypeConnection
pageInfo object____ =
      Object.selectionForCompositeField "pageInfo" [] (object____) (identity >> Decode.nullable)
