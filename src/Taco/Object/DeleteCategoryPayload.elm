-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Object.DeleteCategoryPayload exposing (..)

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

{-| The deteted term object
-}
category : SelectionSet decodesTo Taco.Object.Category
 -> SelectionSet (Maybe decodesTo) Taco.Object.DeleteCategoryPayload
category object____ =
      Object.selectionForCompositeField "category" [] (object____) (identity >> Decode.nullable)


{-| 
-}
clientMutationId : SelectionSet (Maybe String) Taco.Object.DeleteCategoryPayload
clientMutationId =
      Object.selectionForField "(Maybe String)" "clientMutationId" [] (Decode.string |> Decode.nullable)


{-| The ID of the deleted object
-}
deletedId : SelectionSet (Maybe Taco.ScalarCodecs.Id) Taco.Object.DeleteCategoryPayload
deletedId =
      Object.selectionForField "(Maybe ScalarCodecs.Id)" "deletedId" [] (Taco.ScalarCodecs.codecs |> Taco.Scalar.unwrapCodecs |> .codecId |> .decoder |> Decode.nullable)
